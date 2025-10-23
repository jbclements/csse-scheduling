#lang racket

(require data/enumerate
         data/enumerate/lib)

(provide enum-n->term
         enum-term->n)

(define (enum-n->term n)
  (from-nat term/e n))

(define (enum-term->n term)
  (to-nat term/e term))


;; WE ARE DISALLOWING YEARS BEFORE THE BIRTH OF CHRIST.

(define qtr-season/e (fin/e "Winter" "Spring" "Summer" "Fall"))
(define sem-season/e (fin/e "Spring" "Summer" "Fall"))

;; an enumeration of the quarters; all pairs of year/qtr-season
;; where year is below 2027, except for 2026 summer and fall.
(define qtr/e
  (except/e
   (list/e (below/e 2027) qtr-season/e)
   '(2026 "Summer")
   '(2026 "Fall")))

;; an enumeration of the semesters; all pairs of year/sem-season
;; where year is >= 2026, except for 2026 Spring:
(define sem/e
  (except/e
   (list/e (nat+/e 2026) sem-season/e)
   '(2026 "Spring")))

;; I'm responsible for ensuring that the elements here don't overlap.
;; yep, can confirm. It's up to me.

;; staple together qtr/e and sem/e to make term/e
(define term/e
  (append/e qtr/e sem/e))


(module+ test

  (require rackunit)
  
  (check-equal? (to-nat qtr/e '(2026 "Spring"))
                (to-nat term/e '(2026 "Spring")))

  (check-equal? (enum-n->term (enum-term->n '(2026 "Spring")))
                '(2026 "Spring"))

  (define danger-sequence
    '((2025 "Fall") (2026 "Winter") (2026 "Spring") (2026 "Summer") (2026 "Fall")
                    (2027 "Spring") (2027 "Summer") (2027 "Fall")))

  ;; ensure that the result of to-nat on the danger sequence is a sequential
  ;; range of natural numbers:
  (check-equal?
   (map (λ (pr) (enum-term->n pr)) danger-sequence)
   (range (enum-term->n (first danger-sequence))
          (add1 (enum-term->n (last danger-sequence))))))




