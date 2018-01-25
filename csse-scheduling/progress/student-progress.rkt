#lang typed/racket


;; this file provides a list of all the students, using data from the ad-hoc progress report.

;; basically we want units or classes here... these -grades and -versions are basically methods.
(provide Student
         (struct-out Student-Dataset)
         get-students)

(: get-students (String -> Student-Dataset))

(struct Student-Dataset ([students : (Listof Id)]
                         [->grades : (Id -> (Listof Grade-Record))]
                         [->entry-qtr : (Id -> MaybeQtr)]
                         [->expected-graduation-qtr : (Id -> MaybeQtr)]
                         [->major : (Id -> String)]))

(require (only-in "degree-requirements.rkt" Grade-Record Qtr)
         "../credentials.rkt")

;; a student is an id plus a list of grade records
(define-type Student Id)
(define-type Id String)
(define-type MaybeQtr (U Natural False))

(require/typed db/base
               [#:opaque Connection connection?]
               [#:opaque SqlNull sql-null?]
               [query-rows
                (Connection String Any * -> (Listof (Vectorof
                                                     (U String
                                                        Natural
                                                        SqlNull))))])

(require/typed db/postgresql
               [postgresql-connect
                (#:user String #:password String #:port Natural
                 #:database String -> Connection)])



(define-predicate lolostring? (Listof (Listof String)))

;; given a version, get the data.
(define (get-students [version : String]) : Student-Dataset
  (define conn
    (postgresql-connect
     #:user db-username
     #:password db-password
     #:port 13432
     #:database "csseprogress"))


  (define rows : (Listof (Listof (U SqlNull String Natural)))
    (map
     (inst vector->list (U SqlNull String Natural))
     (query-rows
      conn
      (~a "SELECT id,qtr,course,units_earned,grade FROM course_grade"
          " WHERE version=$1;")
      version)))

  (define major-table-query-result
    (query-rows
     conn
     (~a "SELECT m.id,major,e.qtr,g.qtr"
         " FROM ((majors m LEFT JOIN entry_qtr e ON m.id=e.id AND m.version=e.version)"
         "       LEFT JOIN grad_qtr g ON m.id = g.id AND m.version = g.version)"
         " WHERE m.version=$1")
     version))

  (define major-table : (Immutable-HashTable String
                                             (List String
                                                   MaybeQtr
                                                   (U Natural
                                                      'unknown
                                                      'no-data)))
    (make-immutable-hash
     (map
      (λ ([l : (Vectorof (U String Natural SqlNull))])
        : (Pairof String
                  (List String
                        MaybeQtr
                        (U Natural 'unknown 'no-data)))
        (define expected-grad
          (cond [(equal? version "2176-1") 'no-data]
                [else
                 (match (vector-ref l 3)
                   ["NONE" 'unknown]
                   [(and (? string?)
                         (regexp #px"^[0-9]{4}$" (list m)))
                    (assert (string->number m) natural?)])]))
        (cons (assert (vector-ref l 0) string?)
              (list (assert (vector-ref l 1) string?)
                    (match (vector-ref l 2)
                      [(? natural? n) n]
                      [(? sql-null?) #f])
                    expected-grad)))
      major-table-query-result)))

  (define students (hash-keys major-table))


  ;; return the student's current major
  (define (student-major [s : Student]) : String
    (first (hash-ref major-table (student-id s))))

  ;; return the first quarter in which a student took a cal poly class
  (define (student-entry-qtr [s : Student]) : MaybeQtr
    (second (hash-ref major-table (student-id s))))

  ;; return the expected graduation quarter, according to the registrar
  (define (student-expected-graduation-qtr [s : Student]) : MaybeQtr
    (match (third (hash-ref major-table (student-id s)))
      ['no-data (error 'student-expected-graduation-qtr
                       "no expected graduation data for version ~v"
                       version)]
      ['unknown #f]
      [(? natural? n) n]))

  (printf "total rows: ~v\n" (length rows))

  (define (row-id [row : (Listof Any)])
    (cast (first row) String))

  ;; now just the identity function
  (define student-id : (Student -> String) (λ ([x : String]) x))

  (define-predicate grade-record-list? (Listof Grade-Record))

  (define student-grades-table : (Immutable-HashTable Id (Listof Grade-Record))
    (make-immutable-hash
     (map (λ ([g : (Listof (Listof Any))]) : (Pairof Id (Listof Grade-Record))
            (define pre-grade-records : (Listof (Listof Any))
              (map (inst rest Any Any) g))
            (define grade-records
              (for/list : (Listof Grade-Record)
                ([pre-rec (in-list pre-grade-records)])
                (list (assert (first pre-rec) natural?)
                      (assert (second pre-rec) string?)
                      (/ (assert (third pre-rec) real?) 1000)
                      (assert (fourth pre-rec) string?))))
            (cons (row-id (first g)) ; id
                  grade-records ; grades
                  ))
          ;; must add students with no classes here:
          (group-by row-id rows))))

  ;; return the list of student grade records associated with an id
  (define (student-grades [id : Id]) : (Listof Grade-Record)
    (hash-ref student-grades-table id (λ () '())))

  (Student-Dataset students
                   student-grades
                   student-entry-qtr
                   student-expected-graduation-qtr
                   student-major))




;; DEBUGGING CODE TO LOOK AT A PARTICULAR STUDENT:
#;(filter (λ ([s : Student]) (equal? s
                                     ;(substring (first s) 0 5)
                                     "XXX"))
          students)

