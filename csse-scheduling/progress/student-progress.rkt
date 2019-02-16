#lang typed/racket


;; this file provides a list of all the students, using data from the ad-hoc progress report.
;; UPDATE: as of 2018-09-14, it omits grad students

(provide (struct-out Student)
         get-students
         use-localhost?
         student-grades-in-course)

(: get-students (String -> (Listof Student)))

(: use-localhost? (Parameterof Boolean))

(require "../types.rkt"
         "../credentials.rkt")

(define-predicate nn-real? Nonnegative-Real)

;; represents a student
(struct Student ([id : Id]
                 [major : Major-Abbr]
                 [grades : (Listof Grade-Record)]
                 [entry-qtr : (U Qtr 'pre-poly)]
                 [expected-graduation-qtr : (U Qtr 'future 'no-data)])
  #:transparent)

;; a student is an id plus a list of grade records
(define-type Id String)
(define-type MaybeQtr (U Natural False))

(define use-localhost? (make-parameter #f))

(require/typed db/base
               [#:opaque SqlNull sql-null?])

(require/typed "../fetch-mapping.rkt"
               [student-grades-cache
                (String -> (Listof (Pairof String Grade-Record)))]
               [majors-cache
                (String -> (Listof (Vector String String
                                           (U SqlNull Qtr)
                                           String)))])

(define-predicate lolostring? (Listof (Listof String)))

;; given a version, get the data.
(define (get-students [version : String]) : (Listof Student)

  (define rows (student-grades-cache version))

  (when (empty? rows)
    (error 'get-students "no rows. probably bad version: ~e" version))

  

  (define student-grades-table : (Immutable-HashTable Id (Listof Grade-Record))
    (make-immutable-hash
     (map (λ ([g : (Listof (Pairof String Grade-Record))]) : (Pairof Id (Listof Grade-Record))
            (cons ((inst first String) (first g)) ; id
                  (map (inst cdr Any Grade-Record) g) ; grades
                  ))
          (group-by (inst first String) rows))))

  (define major-table-query-result
    (majors-cache version))

  (define students : (Listof Student)
    (map
     (λ ([l : (Vector String String (U SqlNull Qtr) String)]) : Student
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
                (cast (vector-ref l 1) Major-Abbr)
                (hash-ref student-grades-table id (λ () '()))
                (match (vector-ref l 2)
                  [(? natural? n) n]
                  [(? sql-null?) 'pre-poly])
                expected-grad))
     major-table-query-result))

 

  (when (= (length rows) 0)
    (error 'get-students
           "no students found for version ~v"
           version))
  (printf "total rows: ~v\n" (length rows))

  students)

;; given a student and a course, return a list of their grades in that course
(define (student-grades-in-course [s : Student] [c : Course-Id]) : (Listof Grade-Record)
  (filter (λ ([r : Grade-Record]) (equal? (gr-course r) c)) (Student-grades s)))




;; DEBUGGING CODE TO LOOK AT A PARTICULAR STUDENT:
#;(filter (λ ([s : Student]) (equal? s
                                     ;(substring (first s) 0 5)
                                     "XXX"))
          students)

