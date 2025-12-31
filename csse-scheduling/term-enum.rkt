#lang racket

(require data/enumerate
         data/enumerate/lib)

(provide enum-n->term
         enum-term->n
         enum-n->term/no-smr
         enum-term->n/no-smr
         term?
         semester-transition-year
         first-semester-year)

(define (enum-n->term n)
  (from-nat term/e n))

(define (enum-n->term/no-smr n)
  (from-nat term-no-smr/e n))

(define (enum-term->n term)
  (to-nat term/e term))

(define (enum-term->n/no-smr term)
  (to-nat term-no-smr/e term))

(define (term? term)
  ((enum-contract term/e) term))


;; WE ARE DISALLOWING YEARS BEFORE THE BIRTH OF CHRIST.

(define qtr-season/e (fin/e "Winter" "Spring" "Summer" "Fall"))
(define qtr-season-no-smr/e (fin/e "Winter" "Spring" "Fall"))
(define sem-season/e (fin/e "Spring" "Summer" "Fall"))
(define sem-season-no-smr/e (fin/e "Spring" "Fall"))

(define first-semester-year 2026)
(define semester-transition-year first-semester-year)

;; an enumeration of the quarters; all pairs of year/qtr-season
;; where year is below 2027, except for 2026 summer and fall.
(define qtr/e
  (except/e
   (cons/e (below/e (add1 semester-transition-year)) qtr-season/e)
   (cons semester-transition-year "Summer")
   (cons semester-transition-year "Fall")))

;; same thing without summer
(define qtr-no-smr/e
  (except/e
   (cons/e (below/e (add1 semester-transition-year)) qtr-season-no-smr/e)
   (cons semester-transition-year "Fall")))

;; an enumeration of the semesters; all pairs of year/sem-season
;; where year is >= 2026, except for 2026 Spring:
(define sem/e
  (except/e
   (cons/e (nat+/e semester-transition-year) sem-season/e)
   (cons semester-transition-year "Spring")))

;; same thing without summer
(define sem-no-smr/e
  (except/e
   (cons/e (nat+/e semester-transition-year) sem-season-no-smr/e)
   (cons semester-transition-year "Spring")))

;; I'm responsible for ensuring that the elements here don't overlap.
;; yep, can confirm. It's up to me.

;; staple together qtr/e and sem/e to make term/e
(define term/e
  (append/e qtr/e sem/e))

(define term-no-smr/e
  (append/e qtr-no-smr/e sem-no-smr/e))


(module+ test

  (require rackunit)
  
  (check-equal? (to-nat qtr/e '(2026 . "Spring"))
                (to-nat term/e '(2026 . "Spring")))

  (check-equal? (to-nat qtr-no-smr/e '(2026 . "Spring"))
                (to-nat term-no-smr/e '(2026 . "Spring")))

  (check-equal? (enum-n->term (enum-term->n '(2026 . "Spring")))
                '(2026 . "Spring"))
  (check-equal? (enum-n->term/no-smr (enum-term->n/no-smr '(2026 . "Spring")))
                '(2026 . "Spring"))

  (define danger-sequence
    '((2025 . "Fall") (2026 . "Winter") (2026 . "Spring") (2026 . "Summer") (2026 . "Fall")
                    (2027 . "Spring") (2027 . "Summer") (2027 . "Fall")))

  (define danger-sequence/no-smr
    '((2025 . "Fall") (2026 . "Winter") (2026 . "Spring") (2026 . "Fall")
                    (2027 . "Spring")  (2027 . "Fall")))

  ;; ensure that the result of to-nat on the danger sequence is a sequential
  ;; range of natural numbers:
  (check-equal?
   (map (λ (pr) (enum-term->n pr)) danger-sequence)
   (range (enum-term->n (first danger-sequence))
          (add1 (enum-term->n (last danger-sequence)))))

  (check-equal?
   (map (λ (pr) (enum-term->n/no-smr pr)) danger-sequence/no-smr)
   (range (enum-term->n/no-smr (first danger-sequence))
          (add1 (enum-term->n/no-smr (last danger-sequence)))))

  (check-true (flat-contract? (enum-contract term/e)))

  (check-true (term? '(2017 . "Winter")))
  (check-false (term? '(2027 . "Winter"))))




