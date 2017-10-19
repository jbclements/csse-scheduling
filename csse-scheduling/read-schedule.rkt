#lang typed/racket

;; this file parses the schedule-FALLQTR.rktd file, and rearranges
;; the data to show what's happening by quarter by course.
;; it also generates a .csv file, suitable 

(require racket/runtime-path
         "canonicalize.rkt"
         "scheduled-by-csse-dept.rkt"
         "qtr-math.rkt")

(provide schedule-read
         year-schedule
         schedule->records
         sections-equivalent
         year-sections-equivalent
         join-sections-tables
         course-topic?
         canonicalize-topic
         record-course
         record-qtr
         sum-sections
         compress-irec
         record-course-sort-str
         by-num-by-season
         requirement->name)



;; represents a year's teaching assignments.. subtype of Sexp
(define-type Schedule (Listof Instructor))

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

(define-predicate course-topic? CourseTopic)

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



;; given a year, return a sections table for that year
(define (year-sections-equivalent [data-path : Path-String] [fall-year : Natural])
  : SectionsTable
  (sections-equivalent (schedule->records
                        (year-schedule data-path fall-year)
                        (fall-year->catalog-cycle fall-year)
                        (fall-year->base-qtr fall-year))))



(: schedule->records (Schedule CatalogCycle Natural -> (Listof Record)))
(define (schedule->records schedule catalog-cycle base-qtr)
  (unless (= (modulo base-qtr 10) 8)
    (error 'schedule->records
           "expected base quarter ending in 8, got: ~a\v" base-qtr))
  (apply append
         (for/list : (Listof (Listof Record))
           ([irec (in-list schedule)])
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

(: sexp->instructor (Any -> Instructor))
(define (sexp->instructor s)
  (cond [(instructor? s) s]
        [else (raise-argument-error
               'sexp->instructor
               "legal instructor s-expression"
               0 s)]))



(: record-course (Record -> CourseID))
(define record-course fourth)

(: record-qtr (Record -> Natural))
(define record-qtr second)

;; given a year, return the schedule associated with the school year
;; beginning in fall of that year.
(define (year-schedule [data-path : Path-String] [fall-year : Natural])
  : Schedule
  (define input-file (match fall-year
                       [2016 (build-path data-path "schedule-2168.rktd")]
                       [2017 (build-path data-path "schedule-2178.rktd")]))
  (schedule-read input-file))

;; given a filename, read the schedule
(define (schedule-read [input-file : Path]) : Schedule
  (map
   sexp->instructor
   (match (file->value input-file)
     [(? list? l) l]
     [_ (error 'schedule-read
               "expected file containing schedule, got ~e"
               input-file)])))


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
    (match (third irec)
      ['regular 1]
      ['mega2 2]
      ['mega3 3])))


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


