#lang typed/racket

;; functions for mapping back and forth between quarters,
;; fall years, and catalog cycles

(provide CatalogCycle
         fall-year->catalog-cycle
         fall-year->base-qtr
         qtr->fall-year
         qtr->cycle
         qtrs-in-range)

;; extend as needed...
(define-type CatalogCycle
  (U "1995-1997" "1997-1999" "1999-2001" "2001-2003" "2003-2005"
     "2005-2007" "2007-2009" "2009-2011" "2011-2013" "2013-2015"
     "2015-2017" "2017-2019"))

(define-predicate catalog-cycle? CatalogCycle)


(define (fall-year->catalog-cycle [year : Natural]) : CatalogCycle
  (define base-year (- year (modulo (sub1 year) 2)))
  (define result (~a base-year "-" (+ base-year 2)))
  (cond [(catalog-cycle? result) result]
        [else (raise-argument-error
               'fall-year->catalog-cycle
               "year mapping to known cycle (extend list?)"
               0 year)]))


;; map a year to the fall qtr that occurs in it
(define (fall-year->base-qtr [year : Natural]) : Natural
    (cond [(< 1950 year 2150)
           (define century-code : Natural
             (cast (- (floor (/ year 100)) 18)
                   Natural))
           (define year-code
             (modulo year 100))
           (+ (* 1000 century-code) (* 10 year-code) 8)]
          [else (raise-argument-error
                 'fall-year->base-qtr
                 "year between 1950 and 2150"
                 0 year)]))

;; map a qtr to the fall year (summer goes back...)
(define (qtr->fall-year [qtr : Natural]) : Natural
  (define year (+ 2000 (- (floor (/ (- qtr 6) 10)) 200)))
  (cond [(< year 0) (raise-argument-error
                     'qtr->fall-year
                     "qtr falling after the AD 0"
                     0 qtr)]
        [else year]))

;; map a qtr to the cycle that it occurs in
(define (qtr->cycle [qtr : Natural]) : CatalogCycle
  (fall-year->catalog-cycle (qtr->fall-year qtr)))

;; return the quarter numbers greater than or equal
;; to the first quarter and less than the second.
;; ignore summer quarters.
(define (qtrs-in-range [min : Natural] [max : Natural]) : (Listof Natural)
  (for/list ([qtr : Natural (in-range min max)]
             #:when (member (modulo qtr 10) '(2 4 8)))
    qtr))

(module+ test
  (require typed/rackunit)
  (check-equal? (fall-year->base-qtr 2017) 2178)
  (check-equal? (qtr->fall-year 2178) 2017)
  (check-equal? (qtr->fall-year 2176) 2017)
  (check-equal? (qtr->fall-year 2174) 2016)
  (check-equal? (qtr->cycle 2178) "2017-2019")
  (check-equal? (fall-year->catalog-cycle 2006) "2005-2007")

  (check-equal? (qtrs-in-range 2154 2182)
                '(2154 2158 2162 2164 2168 2172 2174 2178)))