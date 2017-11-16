#lang typed/racket

;; functions for mapping back and forth between quarters,
;; fall years, and catalog cycles

(provide CatalogCycle
         fall-year->catalog-cycle
         fall-year->base-qtr
         qtr->fall-year
         qtr->cycle
         qtrs-in-range
         qtr->season
         qtr->year
         year->qtrs)

;; extend as needed...
(define-type CatalogCycle
  (U "1994-1997" "1997-1998" "1998-1999" "1999-2000" "2000-2001"
     "2001-2003" "2003-2005"
     "2005-2007" "2007-2009" "2009-2011" "2011-2013" "2013-2015"
     "2015-2017" "2017-2019"))

(define-type Season
  (U "Winter" "Spring" "Summer" "Fall"))

(define-predicate catalog-cycle? CatalogCycle)

;; given a year (e.g. 2015), return the catalog cycle that
;; fall of that year falls into (in this case, "2015-2017").
(define (fall-year->catalog-cycle [year : Natural]) : CatalogCycle
  ;; weird special-cases
  (cond
    [(<= 1994 year 1996) "1994-1997"]
    [(= year 1997) "1997-1998"]
    [(= year 1998) "1998-1999"]
    [(= year 1999) "1999-2000"]
    [(= year 2000) "2000-2001"]
    [(< 2000 year)
     (define base-year (- year (modulo (sub1 year) 2)))
     (define result (~a base-year "-" (+ base-year 2)))
     (cond [(catalog-cycle? result) result]
           [else (raise-argument-error
                  'fall-year->catalog-cycle
                  "year mapping to known cycle (extend list?)"
                  0 year)])]
    [else (raise-argument-error
           'fall-year->catalog-cycle
           "year mapping to known cycle (extend list?)"
           0 year)]))


;; map a year to the fall qtr that occurs in it (see example below)
(define (fall-year->base-qtr [year : Natural]) : Natural
    (cond [(< 1950 year 2050)
           (define century : Natural
             (assert (floor (/ year 100)) exact-nonnegative-integer?))
           (define century-offset
             (match century
               [19 0]
               [20 2000]))
           (define year-code
             (modulo year 100))
           (+ century-offset (* 10 year-code) 8)]
          [else (raise-argument-error
                 'fall-year->base-qtr
                 "year in range"
                 0 year)]))

;; map a qtr to the fall year (summer goes back...)
(define (qtr->fall-year [qtr : Natural]) : Natural
  (define base-year (qtr->year qtr))
  ;; winter/spring go backward
  (define year (match (qtr->season qtr)
                 [(or "Winter" "Spring") (- base-year 1)]
                 [_ base-year]))
  (cond [(< year 1950) (raise-argument-error
                        'qtr->fall-year
                        "qtr falling after AD 1950"
                        0 qtr)]
        [else year]))

;; map a qtr to the cycle that it occurs in
(define (qtr->cycle [qtr : Natural]) : CatalogCycle
  (fall-year->catalog-cycle (qtr->fall-year qtr)))

;; given a quarter, return its season: "Fall", "Winter", etc.
(define (qtr->season [qtr : Natural]) : Season
  (match (modulo qtr 10)
    [2 "Winter"]
    [4 "Spring"]
    [6 "Summer"]
    [8 "Fall"]))

;; return the year in which a quarter number falls
(define (qtr->year [qtr : Natural]) : Natural
  (define century-code (floor (/ qtr 1000)))
  (define year-code (modulo (floor (/ qtr 10)) 100))
  (define century-offset
    (match century-code
      [0 1900]
      [2 2000]
      [_ (raise-argument-error 'qtr-year
                               "qtr with century code of 0 or 2"
                               0 qtr)]))
  (+ century-offset year-code))


;; return the quarter numbers greater than or equal
;; to the first quarter and less than the second.
;; ignore summer quarters.
(define (qtrs-in-range [min : Natural] [max : Natural]) : (Listof Natural)
  ;; horrible patch for missing century:
  (cond [(<= 1000 min 2000)
         (raise-argument-error 'qtrs-in-range
                               "legal qtr number"
                               0 min)]
        [(<= 1002 max 2000)
         (raise-argument-error 'qtrs-in-range
                               "legal qtr number"
                               0 max)]
        [(and (< min 1000) (< 2000 max))
         (append (qtrs-in-range min 1000)
                 (qtrs-in-range 2002 max))]
        [else
         (for/list ([qtr : Natural (in-range min max)]
                    #:when (member (modulo qtr 10) '(2 4 8)))
           qtr)]
  ))

;; given a year, return the quarters of the academic year beginning in
;; the fall of the given year.
(define (year->qtrs [year : Natural]) : (Listof Natural)
  (qtrs-in-range (fall-year->base-qtr year)
                 (fall-year->base-qtr (add1 year))))

(module+ test
  (require typed/rackunit)

  
  (check-equal? (qtr->year 2018) 2001)
  (check-equal? (qtr->year 976) 1997)
  
  (check-equal? (fall-year->base-qtr 2017) 2178)
  (check-equal? (fall-year->base-qtr 1997) 978)
  (check-equal? (qtr->fall-year 2178) 2017)
  (check-equal? (qtr->fall-year 2176) 2017)
  (check-equal? (qtr->fall-year 2174) 2016)
  (check-equal? (qtr->fall-year 788) 1978)
  
  (check-equal? (qtr->cycle 2178) "2017-2019")
  (check-equal? (qtr->cycle 964)  "1994-1997")
  
  (check-equal? (fall-year->catalog-cycle 2006) "2005-2007")
  (check-equal? (fall-year->catalog-cycle 2000) "2000-2001")
  (check-equal? (fall-year->catalog-cycle 1999) "1999-2000")

  (check-equal? (qtrs-in-range 2154 2182)
                '(2154 2158 2162 2164 2168 2172 2174 2178))
  (check-equal? (qtrs-in-range 982 2014)
                '(982 984 988 992 994 998 2002 2004 2008 2012))

  (check-equal? (qtr->season 2018) "Fall")
  (check-equal? (qtr->season 2102) "Winter")

  (check-equal? (year->qtrs 2016) '(2168 2172 2174))
)