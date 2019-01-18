#lang typed/racket

(provide Qtr
         Course-Id
         Grade
         Units
         Grade-Record
         gr-qtr
         gr-course
         gr-units
         gr-grade)

(define-type Qtr Natural)
(define-type Course-Id String)
(define-type Grade String)
(define-type Units Real)
(define-type Grade-Record (List Qtr Course-Id Units Grade))

(define gr-qtr : (Grade-Record -> Qtr) first)
(define gr-course : (Grade-Record -> Course-Id) second)
(define gr-units : (Grade-Record -> Units) third)
(define gr-grade : (Grade-Record -> String) fourth)