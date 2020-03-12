#lang racket

(require data/enumerate
         data/enumerate/lib)

(provide enum-n->qtr
         enum-qtr->n)

(define (enum-n->qtr n)
  (from-nat qtr/e n))

(define (enum-qtr->n qtr)
  (to-nat qtr/e qtr))

;; WE ARE DISALLOWING YEARS BEFORE THE BIRTH OF CHRIST.

;; these should be defined in one...
(define (nth-season n)
  (match n
    [0 "Winter"]
    [1 "Spring"]
    [2 "Fall"]))

(define (season->n season)
  (match season
    ["Winter" 0]
    ["Spring" 1]
    ["Fall" 2]))

(define (to-pair x)
  (cons (floor (/ x 3))
        (nth-season (remainder x 3))))

(define (from-pair y)
  (+ (* (car y) 3) (season->n (cdr y))))

(define qtr/e
  (map/e to-pair
         from-pair
         natural/e
         #:contract (cons/c natural? (or/c "Winter" "Spring" "Fall"))))

(module+ test
  (require rackunit)
  (check-equal? (from-pair (to-pair 234)) 234)
  (check-equal?
   (for/list ([qtr-n (in-range (to-nat qtr/e '(2014 . "Winter"))
                               (to-nat qtr/e '(2016 . "Fall")))])
     (from-nat qtr/e qtr-n))
   '((2014 . "Winter") (2014 . "Spring") (2014 . "Fall")
                       (2015 . "Winter") (2015 . "Spring") (2015 . "Fall")
                       (2016 . "Winter") (2016 . "Spring"))))

