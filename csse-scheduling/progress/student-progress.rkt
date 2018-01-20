#lang typed/racket


;; this file provides a list of all the students, using data from the ad-hoc progress report.

(provide Student
         students
         student-id
         student-grades
         student-entry-qtr
         student-expected-graduation-qtr
         student-major)

(require (only-in "degree-requirements.rkt" Grade-Record Qtr)
         "../credentials.rkt")

;; a student is an id plus a list of grade records
(define-type Student (List Id (Listof Grade-Record)))
(define-type Id String)

(require/typed db/base
               [#:opaque Connection connection?]
               [query-rows
                (Connection String Any * -> (Listof (Vectorof Any)))])

(require/typed db/postgresql
               [postgresql-connect
                (#:user String #:password String #:port Natural
                 #:database String -> Connection)])

(define conn
  (postgresql-connect
   #:user db-username
   #:password db-password
   #:port 13432
   #:database "csseprogress"))

;; make this ... a parameter? rows will have to be reloaded when it
;; changes. Persistent hash, probably best.
(define version "2178-1")

(define rows : (Listof (Listof Any))
  (map
   (inst vector->list Any)
   (query-rows
    conn
    (~a "SELECT id,qtr,course,grade FROM course_grade"
        " WHERE version=$1;")
    version)))

(define-predicate lolostring? (Listof (Listof String)))

(define major-table-query-result
  (query-rows
     conn
     (~a "SELECT m.id,major,e.qtr,g.qtr"
         " FROM ((majors m LEFT JOIN entry_qtr e ON m.id=e.id AND m.version=e.version)"
         "       LEFT JOIN grad_qtr g ON m.id = g.id AND m.version = g.version)"
         " WHERE m.version=$1")
     version))

(define major-table : (Immutable-HashTable String (List String Natural String))
  (make-immutable-hash
   (map
    (λ ([l : (Vectorof Any)]) : (Pairof String
                                        (List String Natural String))
      (cons (assert (vector-ref l 0) string?)
            (list (assert (vector-ref l 1) string?)
                  (assert (vector-ref l 2) natural?)
                  (assert (vector-ref l 3) string?))))
    major-table-query-result)))


;; return the student's current major
(define (student-major [s : Student]) : String
  (first (hash-ref major-table (student-id s))))

;; return the first quarter in which a student took a cal poly class
(define (student-entry-qtr [s : Student]) : Natural
  (second (hash-ref major-table (student-id s))))

;; return the expected graduation quarter, according to the registrar
(define (student-expected-graduation-qtr [s : Student]) : (U Natural #f)
  (match (third (hash-ref major-table (student-id s)))
    ["NONE" #f]
    [(regexp #px"^[0-9]{4}$" (list m))
     (assert (string->number m) natural?)]
    [other (error 'student-expected-graduation-qtr
                  "unexpected value in expected graduation \
column for student ~v: ~v\n"
                  other)]))

(printf "total rows: ~v\n" (length rows))

(define (row-id [row : (Listof Any)])
  (cast (first row) String))

(define student-id : (Student -> String) first)
(define student-grades : (Student -> (Listof Grade-Record)) second)

(define-predicate grade-record-list? (Listof Grade-Record))

(define students : (Listof Student)
  (map (λ ([g : (Listof (Listof Any))]) : Student
         (list (row-id (first g)) ; id
               (assert (map (inst rest Any Any) g) grade-record-list?) ; grades
               )) 
       (group-by row-id rows)))

;; DEBUGGING CODE TO LOOK AT A PARTICULAR STUDENT:
#;(filter (λ ([s : Student]) (equal? (first s)
                                     ;(substring (first s) 0 5)
                                     "XXX"))
          students)

