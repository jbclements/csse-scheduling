#lang typed/racket


;; this file provides a list of all the students, using data from the ad-hoc progress report.

(provide (struct-out Student)
         get-students)

(: get-students (String -> (Listof Student)))

(require (only-in "degree-requirements.rkt" Grade-Record Qtr)
         "../credentials.rkt")

;; represents a student
(struct Student ([id : Id]
                 [major : String]
                 [grades : (Listof Grade-Record)]
                 [entry-qtr : (U Qtr 'pre-poly)]
                 [expected-graduation-qtr : (U Qtr 'future 'no-data)])
  #:transparent)

;; a student is an id plus a list of grade records
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
(define (get-students [version : String]) : (Listof Student)
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

  (define (row-id [row : (Listof Any)])
    (cast (first row) String))

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
          (group-by row-id rows))))

  (define major-table-query-result
    (query-rows
     conn
     (~a "SELECT m.id,major,e.qtr,g.qtr"
         " FROM ((majors m LEFT JOIN entry_qtr e ON m.id=e.id AND m.version=e.version)"
         "       LEFT JOIN grad_qtr g ON m.id = g.id AND m.version = g.version)"
         " WHERE m.version=$1")
     version))

  (define students : (Listof Student)
    (map
     (λ ([l : (Vectorof (U String Natural SqlNull))]) : Student
       (define expected-grad
         (cond [(equal? version "2176-1") 'no-data]
               [else
                (match (vector-ref l 3)
                  ["NONE" 'future]
                  [(and (? string?)
                        (regexp #px"^[0-9]{4}$" (list m)))
                   (assert (string->number m) natural?)])]))
       (define id (assert (vector-ref l 0) string?))
       (Student id
                (assert (vector-ref l 1) string?)
                (hash-ref student-grades-table id (λ () '()))
                (match (vector-ref l 2)
                  [(? natural? n) n]
                  [(? sql-null?) 'pre-poly])
                expected-grad))
     major-table-query-result))

 


  (printf "total rows: ~v\n" (length rows))

  students)




;; DEBUGGING CODE TO LOOK AT A PARTICULAR STUDENT:
#;(filter (λ ([s : Student]) (equal? s
                                     ;(substring (first s) 0 5)
                                     "XXX"))
          students)

