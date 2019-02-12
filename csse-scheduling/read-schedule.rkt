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
         course-topic
         topic?
         canonicalize-topic
         record-instructor
         record-course
         record-size
         record-qtr
         sum-sections
         compress-irec
         record-course-sort-str
         by-num-by-season
         requirement->name
         Schedule
         availability->total-classroom-wtus)



;; represents a year's teaching assignments.. subtype of Sexp
(define-type Schedule (Pairof Natural (Listof Instructor)))

;; represents an instructor's assignments for the year
(define-type Instructor (List Symbol
                              (Pair 'f Quarter)
                              (Pair 'w Quarter)
                              (Pair 's Quarter)))

;; a quarter's assignment *as it's represented in the schedule-FALLQTR.rktd file*
(define-type Quarter (Listof CourseA))

;; represents a course assignment, optionally mega or 2xmega
(define-type CourseA (U CourseTopic
                  (List 'M CourseTopic)
                  (List 'MM CourseTopic)))

;; represents the size of a course
(define-type CourseSize (U 'regular 'mega2 'mega3))

;; represents the course content (not how many students)
(define-type CourseTopic
  (U Symbol ;; a course id, like 'cpe100 or 'csc520
     Natural ;; just the course number, when it's unambiguous
     ))

;; represents a course's canonical name
(define-type CourseID String)

(define-predicate topic? CourseTopic)

;; represents the data in the style of a database:
;; instructor quarter size courseID
(define-type Record (List Symbol Natural CourseSize CourseID))

;; represents the output of sections-equivalent.
;; e.g.:
#;'(("csc123" (2168 10) (2172 0) (2174 0))
  ("csc481" (2168 2) (2172 2) (2174 2)))
(define-type SectionsTable (Listof SectionsTableRow))
(define-type SectionsTableRow
  (List CourseID (Listof (List Integer Integer))))

;; return the course associated with a table row
(: table-row-course (SectionsTableRow -> CourseID))
(define table-row-course first)

;; return the qtrs associated with a table row
(: table-row-qtrs (SectionsTableRow -> (Listof (List Integer Integer))))
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

(: schedule->records (Schedule -> (Listof Record)))
(define (schedule->records schedule)
  (define instructors (rest schedule))
  (define catalog-cycle (fall-year->catalog-cycle
                         (qtr->fall-year (first schedule))))
  (define base-qtr (first schedule))
  (unless (= (modulo base-qtr 10) 8)
    (error 'schedule->records
           "expected base quarter ending in 8, got: ~a\v" base-qtr))
  (apply append
         (for/list : (Listof (Listof Record))
           ([irec (in-list instructors)])
           (define instructor (first irec))
           (append
            (for/list : (Listof Record)
              ([c (in-list (ann (rest (second irec))
                                (Listof CourseA)))])
              (list instructor base-qtr
                    (course-size c)
                    (canonicalize-topic
                     catalog-cycle
                     (course-topic c))))
            (for/list : (Listof Record)
              ([c (in-list (rest (third irec)))])
              (list instructor (+ base-qtr 4)
                    (course-size c)
                    (canonicalize-topic
                     catalog-cycle
                     (course-topic c))))
            (for/list : (Listof Record)
              ([c (in-list (rest (fourth irec)))])
              (list instructor (+ base-qtr 6)
                    (course-size c)
                    (canonicalize-topic
                     catalog-cycle
                     (course-topic c))))))))

(: course-size (CourseA -> CourseSize))
(define (course-size c)
  (match c
    [(list 'MM _) 'mega3]
    [(list 'M _) 'mega2]
    [other 'regular]))

(define-predicate coursetopic? CourseTopic)

(: course-topic (CourseA -> CourseTopic))
(define (course-topic c)
  (match c
    [(list 'MM t) t]
    [(list 'M t) t]
    [(? coursetopic? t) t]))

(define-predicate nat? Natural)

(: canonicalize-topic (CatalogCycle CourseTopic -> CourseID))
(define (canonicalize-topic cycle topic)
  (match topic
    [(? nat? n) (cast (csc-or-cpe n) CourseID)]
    [(? symbol? s) (ensure-canonical (symbol->string s))]))

(define-predicate instructor? Instructor)

;; remove underscores (they're just placeholders)
(define (strip-placeholders [i : Instructor]) : Instructor
  (list (first i)
        (cons 'f (strip-underscores (rest (second i))))
        (cons 'w (strip-underscores (rest (third i))))
        (cons 's (strip-underscores (rest (fourth i))))))

(define (strip-underscores [cs : Quarter]) : Quarter
  (filter (λ ([c : CourseA]) : Boolean
            (not (equal? c '_)))
          cs))

(: sexp->instructor (Any -> Instructor))
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



;; validate that this is a legal schedule sexp
(define (validate-schedule [schedule-sexp : Sexp]
                           [instructors-to-omit : (Listof Symbol)])
  : Schedule
  (match schedule-sexp
    [(cons (? natural? fall-qtr) (? list? instructor-sexps))
     (define instructors (map sexp->instructor instructor-sexps))
     (define non-omitted (subtract-instructors instructors instructors-to-omit))
     (cons fall-qtr non-omitted)]
    [other (raise-argument-error 'validate-schedule
                                 "list containing fall qtr and list of instructors"
                                 0 schedule-sexp)]))

;; remove records corresponding to the named instructors.
;; signal an error if these instructors don't appear in the list.
(define (subtract-instructors [instructors : (Listof Instructor)]
                              [to-omit : (Listof Symbol)])
  (for ([i (in-list to-omit)])
    (unless (assoc i instructors)
      (error 'subtract-instructors
             "can't omit non-existent instructor: ~e"
             i)))
  (filter (λ ([i : Instructor]) (not (member (first i) to-omit)))
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
                     (List Integer Integer)))
         String)
   (for/list : (Listof (List CourseID
                             (Listof
                              (List Integer Integer))))
     ([course-recs (in-list (by-num-by-season records))])
     (list (record-course (first (first course-recs)))
           (for/list : (Listof
                        (List Natural Integer))
             ([season-recs (in-list course-recs)])
             (list (second (first season-recs))
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
(: sum-sections ((Listof Record) -> Integer))
(define (sum-sections i-recs)
  (for/sum ([irec (in-list i-recs)])
    (record->sections-equivalent irec)))

;; how many sections-worth of students will this course accommodate?
(: record->sections-equivalent (Record -> Integer))
(define (record->sections-equivalent rec)
  (match (third rec)
    ['regular 1]
    ['mega2 2]
    ['mega3 3]))


(define (record-course-sort-str [r : (Pair CourseID Any)])
  : String
  (course-key (car r)))
  
(define-type InstructorAndSize
  (U Symbol
     (List Symbol (U 'm2 'm3))))

  
;; shrink the printed representation of an instructor and size
(: compress-irec (Record -> InstructorAndSize))
(define (compress-irec record)
  (match (third record)
    ['mega2 (list (first record) 'm2)]
    ['mega3 (list (first record) 'm3)]
    ['regular (first record)]))

(define (requirement->name [r : Natural])
  (match r
    [431 (canonicalize "2017-2019" "CSC" "431")]
    [else (csc-or-cpe r)]))



(define-predicate nn-real? Nonnegative-Real)

;; given an availability record, return a total number of wtus
(define (availability->total-classroom-wtus [availability : Sexp]) : Nonnegative-Real
  (match availability
    ['tt-standard 30]
    ['lec-standard 45]
    ['absent 0]
    [(list 'fw (? nn-real? fwwtu)) fwwtu]
    [(list 'total (? nn-real? wtu)) wtu]
    [(list (list 'f (? nn-real? fwtu)) (list 'w (? nn-real? wwtu)) (list 's (? nn-real? swtu)))
     (+ fwtu wwtu swtu)]
    [else
     (raise-argument-error 'availability->total-wtus
                           "known availability format"
                           0 availability)]))