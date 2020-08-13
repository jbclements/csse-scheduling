#lang typed/racket

;; this file parses the content of the schedule file, and rearranges
;; the data to show what's happening by quarter by course.

(require racket/runtime-path
         "canonicalize.rkt"
         "scheduled-by-csse-dept.rkt"
         "qtr-math.rkt")

(provide validate-schedule
         schedule->records
         sections-equivalent
         record->sections-equivalent
         year-sections-equivalent
         join-sections-tables
         courseA-topic
         ;; should have named this courseA-topic:
         course-topic
         courseA-size
         courseA-wtus
         courseA-wtu-override
         topic?
         canonicalize-topic
         Record
         record-instructor
         record-course
         record-size
         record-qtr
         sum-sections
         compress-irec
         record-course-sort-str
         by-num-by-season
         Schedule
         InstructorA
         QuarterA
         CourseA
         availability->total-classroom-wtus)



;; represents a year's teaching assignments.. subtype of Sexp
(define-type Schedule (Pairof Natural (Listof InstructorA)))

;; represents an instructor's assignments for the year
(define-type InstructorA (List Symbol
                              (Pair 'f QuarterA)
                              (Pair 'w QuarterA)
                              (Pair 's QuarterA)))

;; a quarter's assignment *as it's represented in the schedule-FALLQTR.rktd file*
(define-type QuarterA (Listof CourseA))

;; represents a course assignment; use the X to manually specify WTUs. This is the abbreviated
;; format that I use to enter them.
;; examples:
;; 'csc123 : csc 123
;; 123 : also csc 123
;; '(M 123) : a 2x section of 123
;; '(X (MM cpe464) 2) : a 2x section of 123 in which the instructor gets only 2 wtus
;; '(S 430) ; a split course of 430 (not allowing both split and X for now...
(define-type CourseA (U CourseB WTUCourse SplitCourse))

;; a course with an explicit specification of WTUs:
(define-type WTUCourse (List 'X CourseB Nonnegative-Real))
(define-predicate wtu-course? WTUCourse)

;; a course that's split with another instructor:
(define-type SplitCourse (List 'S CourseB))
(define-predicate split-course? SplitCourse)

;; represents a course assignment, optionally mega or 2xmega
;; this format is chosen for ease of entry in schedule.rkt, not as a nice internal representation
(define-type CourseB (U CourseTopic
                        (List 'M CourseTopic)
                        (List 'MM CourseTopic)))

;; represents the size of a course
(define-type CourseSize (U 1 2 3))

;; represents the course content (not how many students)
(define-type CourseTopic
  (U Symbol ;; a course id, like 'cpe100 or 'csc520
     Natural ;; just the course number, when it's unambiguous
     ))

;; represents a course's canonical name
(define-type CourseID String)

(define-predicate topic? CourseTopic)

;; represents the data in the style of a database:
;; instructor quarter size courseID maybe-override-wtus split?
(define-type Record (List Symbol Natural CourseSize CourseID (U False Nonnegative-Real) Boolean))


;; represents the output of sections-equivalent.
;; e.g.:
#;'(("csc123" (2168 10) (2172 0) (2174 0))
  ("csc481" (2168 2) (2172 2) (2174 2)))
(define-type SectionsTable (Listof SectionsTableRow))
(define-type SectionsTableRow
  (List CourseID (Listof (List Qtr Exact-Rational))))

;; return the course associated with a table row
(: table-row-course (SectionsTableRow -> CourseID))
(define table-row-course first)

;; return the qtrs associated with a table row
(: table-row-qtrs (SectionsTableRow -> (Listof (List Qtr Exact-Rational))))
(define table-row-qtrs second)

;; given two sections-tables with non-overlapping qtrs, join them together
(define (join-sections-tables [a : SectionsTable]
                              [b : SectionsTable])
  : SectionsTable
  (define groups (group-by table-row-course (append a b)))
  (for/list : SectionsTable
    ([g (in-list groups)])
    (ann
     (list (table-row-course (first g)) (apply append (map table-row-qtrs g)))
     SectionsTableRow)))

;; given a schedule, return a sections table for that year
(define (year-sections-equivalent [schedule : Schedule]) : SectionsTable
  (sections-equivalent (schedule->records schedule)))

;; given a schedule, validate it and return it as a list of records.
(: schedule->records (Schedule -> (Listof Record)))
(define (schedule->records schedule)
  (define instructor-records (rest schedule))
  (define catalog-cycle (fall-year->catalog-cycle
                         (qtr->fall-year (first schedule))))
  (define base-qtr (first schedule))
  (unless (= (modulo base-qtr 10) 8)
    (error 'schedule->records
           "expected base quarter ending in 8, got: ~a\v" base-qtr))
  (apply append
         (for/list : (Listof (Listof Record))
           ([irec (in-list instructor-records)])
           (define instructor (first irec))
           (apply
            append
            (for/list : (Listof (Listof Record))
              ([qtr-offset : Natural (in-list '(0 4 6))]
               [season (in-list (rest irec))])
              (courseAs->Records catalog-cycle (rest season) instructor (+ base-qtr qtr-offset)))))))

;; map a list of courseA's to a list of Records
(define (courseAs->Records [cycle : CatalogCycle] [cas : (Listof CourseA)] [instructor : Symbol] [qtr : Natural]) : (Listof Record)
  (for/list : (Listof Record)
    ([c (in-list cas)])
    (list instructor qtr
          (courseA-size c)
          (canonicalize-topic
           cycle
           (courseA-topic c))
          (courseA-wtu-override c)
          (courseA-split? c))))

;; is this a split section?
(define (courseA-split? c)
  (cond [(and (list? c) (equal? (first c) 'S)) #t]
        [else #f]))

;; return the courseB contained inside a courseA
(define (courseA->courseB [c : CourseA]) : CourseB
  (cond [(wtu-course? c) (second c)]
        [(split-course? c) (second c)]
        [else c]))

;; if this is an "X" course spec, return the number of wtus
;; specified. Otherwise, return #f
(define (courseA-wtu-override [c : CourseA]) : (U False Nonnegative-Real)
  (cond [(wtu-course? c) (third c)]
        [else #f]))

;; return the "size" of a CourseA. Mega=2, MegaMega=3.
(: courseA-size (CourseA -> CourseSize))
(define (courseA-size c)
  (courseB-size (courseA->courseB c)))

;; return the "size" of a section. Mega=2, MegaMega=3.
(: courseB-size (CourseB -> CourseSize))
(define (courseB-size c)
  (match c
    [(list 'MM _) 3]
    [(list 'M _) 2]
    [other 1]))

(define-predicate coursetopic? CourseTopic)

;; return the "topic"--that is, the course content, independent
;; of how big it is or whether wtu's are specified explicitly
(: courseA-topic (CourseA -> CourseTopic))
(define (courseA-topic c)
  (courseB-topic (courseA->courseB c)))

;; legacy name, not sure where it's used
(define course-topic courseA-topic)

(: courseB-topic (CourseB -> CourseTopic))
(define (courseB-topic c)
  (match c
    [(list 'MM t) t]
    [(list 'M t) t]
    [(? coursetopic? t) t]))

(: canonicalize-topic (CatalogCycle CourseTopic -> CourseID))
(define (canonicalize-topic cycle topic)
  (match topic
    [(? natural? n) (cast (csc-or-cpe n) CourseID)]
    [(? symbol? s) (ensure-canonical (symbol->string s))]))

;; # of wtus for a courseA 
(define (courseA-wtus [this-cycle : CatalogCycle]
                      [courseA : CourseA]) : Real
  ;; warning: multiplier not used in case of explicit wtu override:
  (define course-split-multiplier
    (if (split-course? courseA) 1/2 1))  
  (or (courseA-wtu-override courseA)
      (* course-split-multiplier
         (cycle-course-wtus
          this-cycle
          (canonicalize-topic this-cycle
                              (course-topic courseA))
          (courseA-size courseA)))))

(define-predicate instructor? InstructorA)

;; remove underscores (they're just placeholders)
(define (strip-placeholders [i : InstructorA]) : InstructorA
  (list (first i)
        (cons 'f (strip-underscores (rest (second i))))
        (cons 'w (strip-underscores (rest (third i))))
        (cons 's (strip-underscores (rest (fourth i))))))

(define (strip-underscores [cs : QuarterA]) : QuarterA
  (filter (λ ([c : CourseA]) : Boolean
            (not (equal? c '_)))
          cs))

(: sexp->instructor (Any -> InstructorA))
(define (sexp->instructor s)
  (cond [(instructor? s) (strip-placeholders s)]
        [else (raise-argument-error
               'sexp->instructor
               "legal instructor s-expression"
               0 s)]))


(: record-instructor (Record -> Symbol))
(define record-instructor first)

(: record-course (Record -> CourseID))
(define record-course fourth)

(: record-qtr (Record -> Natural))
(define record-qtr second)

(: record-size (Record -> CourseSize))
(define record-size third)

(define (record-split? [r : Record]) : Boolean
  (sixth r))


;; validate that this is a legal schedule sexp, remove omitted instructors
(define (validate-schedule [schedule-sexp : Sexp]
                           [instructors-to-omit : (Listof Symbol)])
  : Schedule
  (match schedule-sexp
    [(cons (? natural? fall-qtr) (? list? instructor-sexps))
     (define instructors (map sexp->instructor instructor-sexps))
     (define non-omitted (subtract-instructors instructors instructors-to-omit))
     (define maybe-duplicated-instructor (check-duplicates (map (ann first (-> InstructorA Symbol))
                                                                instructors)))
     (when maybe-duplicated-instructor
       (error 'validate-schedule "instructor appears more than once: ~e\n"
              maybe-duplicated-instructor))
     (cons fall-qtr non-omitted)]
    [other (raise-argument-error 'validate-schedule
                                 "list containing fall qtr and list of instructors"
                                 0 schedule-sexp)]))

;; remove records corresponding to the named instructors.
;; signal an error if these instructors don't appear in the list.
(define (subtract-instructors [instructors : (Listof InstructorA)]
                              [to-omit : (Listof Symbol)])
  (for ([i (in-list to-omit)])
    (unless (assoc i instructors)
      (error 'subtract-instructors
             "can't omit non-existent instructor: ~e"
             i)))
  (filter (λ ([i : InstructorA]) (not (member (first i) to-omit)))
          instructors))


(module+ test
  (require typed/rackunit)

  (check-equal? (subtract-instructors '((a (f) (w) (s))
                                        (b (f) (w) (s))
                                        (c (f) (w) (s))
                                        (d (f) (w) (s))
                                        (e (f) (w) (s)))
                                      '(d b))
                '((a (f) (w) (s))
                  (c (f) (w) (s))
                  (e (f) (w) (s))))

  (check-exn #px"non-existent instructor"
             (λ ()
               (subtract-instructors '((a (f) (w) (s))
                                       (b (f) (w) (s))
                                       (c (f) (w) (s))
                                       (d (f) (w) (s))
                                       (e (f) (w) (s)))
                                     '(d q)))))


;; given a list of records, return the # of sections of each course
;; to be offered in each quarter.
(define (sections-equivalent [records : (Listof Record)])
  ((inst sort (List CourseID
                    (Listof
                     (List Qtr Exact-Rational)))
         String)
   (for/list : (Listof (List CourseID
                             (Listof
                              (List Qtr Exact-Rational))))
     ([course-recs (in-list (by-num-by-season records))])
     (list (record-course (first (first course-recs)))
           (for/list : (Listof
                        (List Qtr Exact-Rational))
             ([season-recs (in-list course-recs)])
             (list (record-qtr (first season-recs))
                   (sum-sections season-recs)))))
   string<?
   #:key record-course-sort-str))

;; given a list of records, group them by number and season
(define (by-num-by-season [records : (Listof Record)])
  : (Listof (Listof (Listof Record)))
  (map
   (λ ([recs : (Listof Record)])
     (group-by (ann second (Record -> Natural))
               recs))
   (group-by record-course records)))

;; how many sections-worth of students will this accommodate?
(: sum-sections ((Listof Record) -> Exact-Rational))
(define (sum-sections i-recs)
  (for/sum ([irec (in-list i-recs)])
    (record->sections-equivalent irec)))

;; how many sections-worth of students will this course accommodate?
(: record->sections-equivalent (Record -> CourseSize))
(define (record->sections-equivalent r)
  (define split-multiplier
    (if (record-split? r) 1/2 1))
  ;; I believe this math is too hard for TR:
  (cast
   (* (record-size r)
      split-multiplier)
   CourseSize))

(define (record-course-sort-str [r : (Pair CourseID Any)])
  : String
  (course-key (car r)))

(define-type InstructorAndSize
  (U InstructorAndSize2
     (List 'split InstructorAndSize2)))

(define-type InstructorAndSize2
  (U Symbol
     (List Symbol (U 'm2 'm3))))

  
;; shrink the printed representation of an instructor and size
(: compress-irec (Record -> InstructorAndSize))
(define (compress-irec record)
  (define with-size : InstructorAndSize2
    (match (record-size record)
      [2 (list (first record) 'm2)]
      [3 (list (first record) 'm3)]
      [1 (first record)]))
  (cond [(record-split? record)
         (list 'split with-size)]
        [else with-size]))

(define-predicate nn-real? Nonnegative-Real)

;; given an availability record, return a total number of wtus
(define (availability->total-classroom-wtus [availability : Sexp]) : Nonnegative-Real
  (match availability
    ['tt-standard 30]
    ['tt-first-year 20]
    ['tt-second-year 20]
    ['lec-standard 45]
    ['absent 0]
    [(list 'fall-winter (? nn-real? fwwtu)) fwwtu]
    [(list 'winter-spring (? nn-real? wswtu)) wswtu]
    [(list 'total (? nn-real? wtu)) wtu]
    [(list (list 'f (? nn-real? fwtu)) (list 'w (? nn-real? wwtu)) (list 's (? nn-real? swtu)))
     (+ fwtu wwtu swtu)]
    [else
     (raise-argument-error 'availability->total-wtus
                           "known availability format"
                           0 availability)]))

(module+ test
  (require typed/rackunit)

  (define s1
    '(2218
      (zippy (f 349) (w) (s 430))
      (cronkite (f 101 231 231 231) (w cpe450) (s (X (MM 490) 22)))))

  (check-equal? (validate-schedule s1 '()) s1)
  (check-equal? (availability->total-classroom-wtus 'lec-standard) 45)
  (check-equal? (availability->total-classroom-wtus '(fall-winter 3.3)) 3.3)

  )

 
(module+ test
  (require typed/rackunit)
  (check-equal? (courseA-wtus "2019-2020" 430) 5.0)
  (check-equal? (courseA-wtus "2019-2020" '(MM 430)) 9.0)
  (check-equal? (courseA-wtus "2019-2020" '(X (MM 430) 3.2)) 3.2))
