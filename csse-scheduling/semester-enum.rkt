#lang racket

(require data/enumerate
         data/enumerate/lib)

;; s-term : semester term
(provide enum-n->s-term
         enum-s-term->n)

(define (enum-n->s-term n)
  (from-nat s-term/e n))

(define (enum-s-term->n qtr)
  (to-nat s-term/e qtr))


;; could definitely abstract between this and qtr-enum-nosummer.rkt...

;; WE ARE DISALLOWING YEARS BEFORE THE BIRTH OF CHRIST.

;; these should be defined in one...
(define (nth-season n)
  (match n
    [0 "Spring"]
    [1 "Summer"]
    [2 "Fall"]))

(define (season->n season)
  (match season
    ["Spring" 0]
    ["Summer" 1]
    ["Fall" 2]))

;; convert a number to a year/season pair
(define (to-pair x)
  (cons (floor (/ x 3))
        (nth-season (remainder x 3))))

(define (from-pair y)
  (+ (* (car y) 3) (season->n (cdr y))))

(define s-term/e
  (map/e to-pair
         from-pair
         natural/e
         #:contract (cons/c natural? (or/c "Spring" "Summer" "Fall"))))

(module+ test
  (require rackunit)
  (check-equal? (from-pair (to-pair 234)) 234)
  (check-equal?
   (for/list ([s-term-n (in-range (to-nat s-term/e '(2034 . "Spring"))
                               (to-nat s-term/e '(2036 . "Summer")))])
     (from-nat s-term/e s-term-n))
   '((2034 . "Spring") (2034 . "Summer") (2034 . "Fall")
                       (2035 . "Spring") (2035 . "Summer") (2035 . "Fall")
                       (2036 . "Spring"))))

