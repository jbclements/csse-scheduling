#lang typed/racket/base

;; this file parses the content of the schedule file, and rearranges
;; the data to show what's happening by quarter by course.

(require racket/runtime-path
         "canonicalize.rkt"
         "scheduled-by-csse-dept.rkt"
         "qtr-math.rkt"
         [only-in racket/list first second third fourth fifth sixth rest
                  group-by check-duplicates filter-map]
         racket/match
         [only-in racket/math natural?])

(provide validate-schedule
         validate-semester-schedule
         schedule->records
         sections-equivalent
         record->sections-equivalent
         record->wtus
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
         record-filter
         by-num-by-season
         Schedule
         SemSchedule
         InstructorA
         InstructorSA
         QuarterA
         CourseA
         course-a?
         assigned-time-flatten)



;; appears in schedules.rkt as well... I think with dynamic require
;; I just have to put it in both places...
(define-type Omits (Listof (U Symbol (List Symbol (Listof (U 'f 'w 's))))))

;; represents a year's teaching assignments.. subtype of Sexp
(define-type Schedule (Pairof Natural (Listof InstructorA)))
(define-type SemSchedule (Pairof Natural (Listof InstructorSA)))

;; represents an instructor's assignments for the year
(define-type InstructorA (Pair Symbol InstructorASched))
(define-type InstructorASched (List (Pair 'f QuarterA)
                                    (Pair 'w QuarterA)
                                    (Pair 's QuarterA)))
;; same for semesters
(define-type InstructorSA (Pair Symbol InstructorSASched))
(define-type InstructorSASched (List (Pair 'f QuarterA)
                                     (Pair 's QuarterA)))

;; a quarter's assignment *as it's represented in the schedule-FALLQTR.rktd file*
(define-type QuarterA (Listof CourseA))

;; An assignment is either a CourseA or a Release
(define-type Assignment (U CourseA Release))

(define-type Release (list 'R Nonnegative-))

;; CourseA : represents a course assignment; use the X to manually specify WTUs. This is the abbreviated
;; format that I use to enter them.
;; examples:
;; 'csc123 : csc 123
;; 123 : also csc 123
;; '(M 123) : a 2x section of 123
;; '(X (MM cpe464) 2) : a 2x section of 123 in which the instructor gets only 2 wtus
;; '(S 430) ; a split course of 430 (not allowing both split and X for now...
;; '(R 3.5) ;; release time of 3.5 WTUs
(define-type CourseA (U NoprintCourse CourseRelease MaybeSplitCourse))
(define-predicate course-a? CourseA)

;; a course that's set to not print ... but actually, I don't think I actually use this?
(define-type NoprintCourse (List 'N MaybeSplitCourse))
(define-predicate noprint-course? NoprintCourse)

(define-type MaybeSplitCourse (U SplitCourse MaybeWTUCourse))
;; a course that's split with another instructor:
(define-type SplitCourse (List 'S MaybeWTUCourse))
(define-predicate split-course? SplitCourse)

(define-type MaybeWTUCourse (U WTUCourse WTURelease CourseB))
;; a course with an explicit specification of WTUs:
(define-type WTUCourse (List 'X CourseB Nonnegative-Exact-Rational))
(define-type WTURelease (List 'R Nonnegative-Exact-Rational))
(define-predicate wtu-course? WTUCourse)

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
(define-type Record (List Symbol Natural CourseSize CourseID (U False Nonnegative-Exact-Rational) Boolean))


;; represents the output of sections-equivalent.
;; e.g.:
#;'(("csc123" (2168 10) (2172 0) (2174 0))
  ("csc481" (2168 2) (2172 2) (2174 2)))
(define-type SectionsTable (Listof SectionsTableRow))
(define-type SectionsTableRow
  (List CourseID (Listof (List CPTN Exact-Rational))))

;; return the course associated with a table row
(: table-row-course (SectionsTableRow -> CourseID))
(define table-row-course first)

;; return the qtrs associated with a table row
(: table-row-qtrs (SectionsTableRow -> (Listof (List CPTN Exact-Rational))))
(define table-row-qtrs second)
(: table-row-terms (SectionsTableRow -> (Listof (List CPTN Exact-Rational))))
(define table-row-terms second)


;; given two sections-tables with non-overlapping terms, join them together
(define (join-sections-tables [a : SectionsTable]
                              [b : SectionsTable])
  : SectionsTable
  (define groups (group-by table-row-course (append a b)))
  (for/list : SectionsTable
    ([g (in-list groups)])
    (ann
     (list (table-row-course (first g)) (apply append (map table-row-terms g)))
     SectionsTableRow)))

;; given a schedule, return a sections table for that year
(define (year-sections-equivalent [schedule : Schedule]) : SectionsTable
  (sections-equivalent (schedule->records schedule)))

;; semester version. Type differences percolate through?
(define (schedule->records [schedule : (U Schedule SemSchedule)])
  : (Listof Record)
  (define instructor-records (rest schedule))
  (define catalog-cycle (fall-year->catalog-cycle
                         (term->fall-year (first schedule))))
  (define base-term (first schedule))
  (unless (equal? (term->season base-term) "Fall")
    (error 'schedule->records
           "expected base quarter which is Fall,  got: ~a\v" base-term))
  (apply append
         (for/list : (Listof (Listof Record))
           ([irec instructor-records])
           (define instructor (first irec))
           (apply
            append
            (for/list : (Listof (Listof Record))
              ([season (rest irec)])
              (define term (season-after-term (coerce-season (first season)) base-term))
              (courseAs->Records catalog-cycle (rest season) instructor term))))))

;; map a list of courseA's to a list of Records
(define (courseAs->Records [cycle : CatalogCycle] [cas : (Listof CourseA)]
                           [instructor : Symbol] [qtr : Natural]) : (Listof Record)
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
(: courseA-split? (CourseA -> Boolean : SplitCourse))
(define (courseA-split? c) 
  (and (list? c) (equal? (first c) 'S)))

(: courseA-wtuspec? (CourseA -> Boolean : WTUCourse))
(define (courseA-wtuspec? c)
  (and (list? c) (equal? (first c) 'X)))

;; return the courseB contained inside a courseA
(define (courseA->courseB [c : CourseA]) : CourseB
  (define stripped
    (cond [(noprint-course? c) (second c)]
          [else c]))
  (MaybeSplitCourse->courseB stripped))

(define (MaybeSplitCourse->courseB [c : MaybeSplitCourse]) : CourseB
  (MaybeWTUCourse->courseB
   (cond [(courseA-split? c) (second c)]
         [else c])))

(define (MaybeWTUCourse->courseB [c : MaybeWTUCourse]) : CourseB
  (cond [(courseA-wtuspec? c) (second c)]
        [else c]))


;; if this is an "X" course spec, return the number of wtus
;; specified. Otherwise, return #f
(define (courseA-wtu-override [c : CourseA]) : (U False Nonnegative-Exact-Rational)
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
    [(? natural? n) (cast (canonicalize/num cycle n) CourseID)]
    [(? symbol? s) (ensure-canonical (symbol->string s))]))

;; # of wtus for a courseA 
(define (courseA-wtus [this-cycle : CatalogCycle]
                      [courseA : CourseA]) : Real
  (course-wtus-helper
   this-cycle
   (canonicalize-topic this-cycle
                       (course-topic courseA))
   (split-course? courseA)
   (courseA-wtu-override courseA)
   (courseA-size courseA)))

;; used both by courseA-wtus and record->wtus
(define (course-wtus-helper [this-cycle : CatalogCycle]
                            [id : Course-Id]
                            [split? : Boolean]
                            [maybe-override : (U False
                                                 Nonnegative-Exact-Rational)]
                            [size : Natural])
  (when (and maybe-override split?)
    ;; you can lift this restriction, you just need to decide
    ;; whether the nesting order matters, and what it means.
    (error 'course-wtus-helper
           "split and override not allowed simultaneously."))
  (define course-split-multiplier
    (if split? 1/2 1))  
  (or maybe-override
      (* course-split-multiplier
         (cycle-course-wtus
          this-cycle
          id
          size))))

(define-predicate instructor? InstructorA)
(define-predicate sem-instructor? InstructorSA)

;; remove underscores (they're just placeholders)
(define (strip-placeholders [i : InstructorA]) : InstructorA
  (list (first i)
        (cons 'f (strip-underscores (rest (second i))))
        (cons 'w (strip-underscores (rest (third i))))
        (cons 's (strip-underscores (rest (fourth i))))))

(define (sem-strip-placeholders [i : InstructorSA]) : InstructorSA
  (list (first i)
        (cons 'f (strip-underscores (rest (second i))))
        (cons 's (strip-underscores (rest (third i))))))

(define (strip-underscores [cs : QuarterA]) : QuarterA
  (filter (λ ([c : CourseA]) : Boolean
            (not (equal? c '_)))
          cs))

;; do type-classes allow abstraction over the following pair of functions?
(: sexp->instructor (Any -> InstructorA))
(define (sexp->instructor s)
  (cond [(instructor? s) (strip-placeholders s)]
        [else (raise-argument-error
               'sexp->instructor
               "legal instructor s-expression"
               0 s)]))

(: sexp->sem-instructor (Any -> InstructorSA))
(define (sexp->sem-instructor s)
  (cond [(sem-instructor? s) (sem-strip-placeholders s)]
        [else (raise-argument-error
               'sexp->sem-instructor
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

(define (record-maybe-override-wtus [r : Record])
  : (U False Nonnegative-Exact-Rational)
  (fifth r))

(define (record-split? [r : Record]) : Boolean
  (sixth r))



;; validate that this is a legal schedule sexp, remove omitted instructors, return a schedule
;; (which is still an S-expression. IOW, this function can be the identity.
(define (validate-schedule [schedule-sexp : Sexp]
                           [instructors-to-omit : Omits])
  : Schedule
  ((inst validate-schedule-helper InstructorA)
   schedule-sexp sexp->instructor first instructors-to-omit subtract-some-quarters))

(define (validate-semester-schedule [schedule-sexp : Sexp]
                                    [instructors-to-omit : Omits])
  : SemSchedule
  ((inst validate-schedule-helper InstructorSA)
   schedule-sexp sexp->sem-instructor first instructors-to-omit sem-subtract-some-quarters))

;; abstraction for the above two allows either kind of Instructor type.
(: validate-schedule-helper (All (IType) (Sexp (Sexp -> IType) (IType -> Symbol) Omits (IType (Listof Symbol) -> IType) -> (Pairof Natural (Listof IType)))))
(define (validate-schedule-helper schedule-sexp translator instructor-name instructors-to-omit subtractor)
  (match schedule-sexp
    [(cons (? natural? fall-qtr) (? list? instructor-sexps))
     (define instructors (map translator instructor-sexps))
     (define non-omitted ((inst subtract-instructors IType) instructors instructor-name instructors-to-omit subtractor))
     (define maybe-duplicated-instructor (check-duplicates (map instructor-name instructors)))
     (when maybe-duplicated-instructor
       (error 'validate-schedule "instructor appears more than once: ~e\n"
              maybe-duplicated-instructor))
     (cons fall-qtr non-omitted)]
    [other (raise-argument-error 'validate-schedule
                                 "list containing fall qtr and list of instructors"
                                 0 schedule-sexp)]))


;; remove records corresponding to the named instructors.
;; signal an error if these instructors don't appear in the list.
(: subtract-instructors (All (IType) ((Listof IType)
                                      (IType -> Symbol) Omits
                                      (IType (Listof Symbol) -> IType)
                                      -> (Listof IType))))
(define (subtract-instructors instructors instructor-name to-omit subtractor)
  (define to-omit-names
    (map (λ ([i : (U Symbol (List Symbol Any))])
           (cond [(symbol? i) i]
                 [else (first i)]))
         to-omit))
  (define partial-year-omits
    (filter (ann pair? (-> (U Symbol (List Symbol (Listof Symbol)))
                           Boolean : (List Symbol (Listof Symbol))))
            to-omit))
  (for ([name (in-list to-omit-names)])
    (unless (findf (λ ([i : IType]) (equal? (instructor-name i) name))
                   instructors)
      (error 'subtract-instructors
             "can't omit non-existent instructor: ~e"
             name)))
  (filter-map
   (λ ([i : IType]) : (U False IType)
     (cond [(member (instructor-name i) to-omit-names)
            ;; could be whole-year, or just some quarters
            (cond [(member (instructor-name i) to-omit)
                   ;; whole year... just drop it
                   #f]
                  [else
                   (match (assoc (instructor-name i) partial-year-omits)
                     [#f (error 'subtract-instructors "internal error, can't find record")]
                     [(list _ quarters) (subtractor i quarters)])])]
           [else
            i]))
   instructors))


(define (subtract-some-quarters [i : InstructorA] [drop-quarters : (Listof Symbol)])
  : InstructorA
  (list (first i)
        (cons 'f (maybe-drop (rest (second i)) 'f drop-quarters))
        (cons 'w (maybe-drop (rest (third i)) 'w drop-quarters))
        (cons 's (maybe-drop (rest (fourth i)) 's drop-quarters))))


(define (sem-subtract-some-quarters [i : InstructorSA] [drop-quarters : (Listof Symbol)])
  : InstructorSA
  (list (first i)
        (cons 'f (maybe-drop (rest (second i)) 'f drop-quarters))
        (cons 's (maybe-drop (rest (third i)) 's drop-quarters))))

(define (maybe-drop [qtr : (Listof CourseA)] [qtr-sym : Symbol] [to-drop : (Listof Symbol)])
  : (Listof CourseA)
  (cond [(member qtr-sym to-drop) '()]
        [else qtr]))

(module+ test
  (require typed/rackunit)

  (check-equal? ((inst subtract-instructors InstructorA)
                 '((a (f 1) (w 3) (s 4))
                   (b (f 2) (w 4) (s 3))
                   (c (f 3) (w 5) (s 9))
                   (d (f 4) (w 6) (s 1234))
                   (e (f 5) (w 8) (s 17)))
                 first
                 '(d (e (w)) b)
                 subtract-some-quarters)
                '((a (f 1) (w 3) (s 4))
                  (c (f 3) (w 5) (s 9))
                  (e (f 5) (w) (s 17))))

  (check-exn #px"non-existent instructor"
             (λ ()
               ((inst subtract-instructors InstructorA)
                '((a (f) (w) (s))
                  (b (f) (w) (s))
                  (c (f) (w) (s))
                  (d (f) (w) (s))
                  (e (f) (w) (s)))
                first
                '(d q)
                subtract-some-quarters))))


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
  (for/sum : Exact-Rational ([irec (in-list i-recs)])
    (record->sections-equivalent irec)))

;; how many sections-worth of students will this course accommodate?
(: record->sections-equivalent (Record -> Exact-Rational))
(define (record->sections-equivalent r)
  (define split-multiplier
    (if (record-split? r) 1/2 1))
  (* (record-size r)
     split-multiplier))

;; how many wtus does is this course worth?
(: record->wtus (CatalogCycle Record -> Exact-Rational))
(define (record->wtus cycle r)
  (course-wtus-helper cycle
                      (record-course r)
                      (record-split? r)
                      (record-maybe-override-wtus r)
                      (record-size r)))

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

;; given an assigned time spec, return either a real representing flexible
;; assigned time to be distributed across the year, or a list of three
;; reals indicating the amount of assigned time for each quarter.
(define (assigned-time-flatten [assigned-time-spec : Sexp]) : (U Real (List Real Real Real))
  (match assigned-time-spec
    [(list 'total (? nn-real? wtu)) wtu]
    [(list (list (or 'f 'w 's) (? real?)) ...)
     ;; cast must succeed by pattern above...
     (define seasons (cast assigned-time-spec (Listof (List Symbol Real))))
     (when (check-duplicates (map (inst first Symbol) seasons))
       (error 'assigned-time-flatten "duplicated season name: ~v" assigned-time-spec))
     ;; I don't think TR can reason about the length of the iterated list like this
     (cast
      (for/list : (Listof Real) ([season-name (in-list '(f w s))])
        (define assoc-result (assoc season-name seasons))
        (cond [(not assoc-result) 0]
              [else ((inst cadr Any Real) assoc-result)]))
      (List Real Real Real))]
    [other
     (error 'assigned-time-flatten
            "unexpected assigned-time format: ~v"
            other)]))

(define (semester-assigned-time-flatten [assigned-time-spec : Sexp]) : (U Real (List Real Real))
  (match (assigned-time-flatten assigned-time-spec)
    [(? real? r) r]
    [(list (? real? f) (? real? w) (? real? s))
     (unless (= w 0)
       (error 'semester-assigned-time-flatten
              "expected winter assigned time to be zero under semesters, got: ~e"
              w))
     (list f s)]))

;; select only records in particular quarters or with a given name
(define (record-filter [records : (Listof Record)]
                       #:course [course : (U False CourseID) #f]
                       #:qtr [qtrs : (U False (Listof Qtr)) #f]) : (Listof Record)
  (define qtr-filter : (Record -> Boolean)
    (cond [(not qtrs) (λ ([r : Record]) #t)]
          [else (λ ([r : Record]) (and (member (record-qtr r) qtrs) #t))]))
  (define course-filter : (Record -> Boolean)
    (cond [(not course) (λ ([r : Record]) #t)]
          [else (λ ([r : Record]) (equal? (record-course r) course))]))
  (filter course-filter (filter qtr-filter records)))

(module+ test
  (require typed/rackunit)

  (define s1
    '(2218
      (zippy (f 349) (w) (s 430))
      (cronkite (f 101 231 231 231) (w cpe450) (s (X (MM 490) 22)))))

  (check-equal? (validate-schedule s1 '()) s1)

  (check-equal? (assigned-time-flatten '(total 19)) 19)
  (check-equal? (assigned-time-flatten '((w 3) (s 2))) '(0 3 2))

  (check-equal? (semester-assigned-time-flatten '(total 19)) 19)
  (check-equal? (semester-assigned-time-flatten '((f 3) (s 2))) '(3 2))
  (check-exn #px"winter assigned time to be zero"
             (λ () (semester-assigned-time-flatten '((w 3) (s 2)))))
  

  )

 
(module+ test
  (require typed/rackunit)
  (check-equal? (courseA-wtus "2019-2020" 430) 5)
  (check-equal? (courseA-wtus "2019-2020" '(MM 430)) 9)
  (check-equal? (courseA-wtus "2019-2020" '(X (MM 430) #e3.2)) #e3.2)
  (check-equal? (courseA-wtus "2026-2028" '(R 3)) 3))
