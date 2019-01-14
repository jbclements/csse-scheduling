#lang racket

(require data/enumerate
         data/enumerate/lib)

(define (even-mod-10? x) (= 0 (modulo x 10)))

(define tens/e
  (map/e (λ (x) (* x 10))
         (λ (x) (/ x 10))
         natural/e
         #:contract (and/c natural?
                           even-mod-10?)))

(enum->list tens/e 10)

(define (shifted enum enum-pred n)
  (map/e (λ (x) (+ x n))
         (λ (x) (- x n))
         enum
         #:contract (λ (x) (enum-pred (- x n)))))

(define winters/e (shifted tens/e even-mod-10? 2))
(define springs/e (shifted tens/e even-mod-10? 4))
(define summers/e (shifted tens/e even-mod-10? 6))
(define falls/e   (shifted tens/e even-mod-10? 8))

(define pre-qtrs/e (or/e winters/e springs/e summers/e falls/e))

(define qtrs/e
  (but-not/e pre-qtrs/e (range/e 1000 1100)))

(to-nat pre-qtrs/e 2148)

(enum->list qtrs/e 10)

(to-nat qtrs/e 2148)

(define test/e (but-not/e tens/e (range/e 1000 2000)))
(enum->list test/e 10)

(to-nat test/e 10)



