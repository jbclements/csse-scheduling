#lang typed/racket/base

;; functions for mapping back and forth between quarters,
;; fall years, and catalog cycles

(require racket/match
         "./types.rkt"
         (only-in racket/list range))

(provide CatalogCycle
         catalog-cycle?
         fall-year->catalog-cycle
         catalog-cycle->fall-years
         fall-year->base-qtr
         qtr->fall-year
         qtr->catalog-cycle
         qtrs-in-range
         qtr->season
         qtr->year
         qtr->string
         string->qtr
         year->qtrs
         catalog-cycle->qtrs
         encode-qtr
         season-after-qtr
         Season)

(define first-encodable-year : Natural 1900)

;; this is a little D.R.Y. macro to avoid listing
;; the names of the cycles twice.`
(define-syntax cycle-defs
  (syntax-rules ()
    [(_ type-name list-name cycle ...)
     (begin
       (define-type type-name (U cycle ...))
       (define list-name (list cycle ...)))]))

;; extend as needed...
(cycle-defs
 CatalogCycle
 all-cycles
 "1994-1997" "1997-1998" "1998-1999" "1999-2000" "2000-2001"
 "2001-2003" "2003-2005"
 "2005-2007" "2007-2009" "2009-2011" "2011-2013" "2013-2015"
 "2015-2017" "2017-2019" "2019-2020")

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
     (define result (string-append (number->string base-year)
                                   "-"
                                   (number->string (+ base-year 2))))
     (cond [(catalog-cycle? result) result]
           [else (raise-argument-error
                  'fall-year->catalog-cycle
                  "year mapping to known cycle (extend list?)"
                  0 year)])]
    [else (raise-argument-error
           'fall-year->catalog-cycle
           "year mapping to known cycle (extend list?)"
           0 year)]))

;; given a cycle (e.g. "2015-2017", return the fall years that
;; map to it
(define (catalog-cycle->fall-years [cycle : CatalogCycle]) : (Listof Natural)
  (match cycle
    [(regexp #px"^(\\d{4})-(\\d{4})$" (list _ startstr endstr))
     (define start-year (assert (string->number (assert startstr string?)) natural?))
     (define end-year (assert (string->number (assert endstr string?)) natural?))
     (range start-year end-year)]))

(define natural? exact-nonnegative-integer?)

;; map a year to the fall qtr that occurs in it (see example below)
(define (fall-year->base-qtr [year : Natural]) : Qtr
  (encode-qtr year "Fall"))

;; map a qtr to the fall year (summer goes forward...)
(define (qtr->fall-year [qtr : Qtr]) : Natural
  (define base-year (qtr->year qtr))
  ;; winter/spring go backward
  (match (qtr->season qtr)
    [(or "Winter" "Spring")
     (define year (- base-year 1))
     (cond [(<= first-encodable-year year) year]
           [else (raise-argument-error 'qtr->fall-year
                                       "qtr whose fall year is encodable"
                                       0 qtr)])]
    ;; Fall/Summer stay the same.
    [_ base-year]))

;; map a qtr to the cycle that it occurs in
(define (qtr->catalog-cycle [qtr : Qtr]) : CatalogCycle
  (fall-year->catalog-cycle (qtr->fall-year qtr)))

;; given a string or symbol, coerce it to a season if possible
(define (coerce-season [s : (U Symbol String)]) : Season
  (match s
    [(or 'winter "winter" "Winter") "Winter"]
    [(or 'spring "spring" "Spring") "Spring"]
    [(or 'summer "summer" "Summer") "Summer"]
    [(or 'fall "fall" "Fall") "Fall"]
    [other
     (raise-argument-error 'coerce-season
                           "season-like symbol or string"
                           0 s)]))

;; given a quarter, return its season: "Fall", "Winter", etc.
(define (qtr->season [qtr : Qtr]) : Season
  (match (modulo qtr 10)
    [2 "Winter"]
    [4 "Spring"]
    [6 "Summer"]
    [8 "Fall"]))

;; given a season, return its quarter offset
(define (season->qtr-offset [season : Season]) : Natural
  (match season
    ["Winter" 2]
    ["Spring" 4]
    ["Summer" 6]
    ["Fall" 8]))

;; return the year in which a quarter number falls
(define (qtr->year [qtr : Qtr]) : Natural
  (define century-code (floor (/ qtr 1000)))
  (define year-code (modulo (floor (/ qtr 10)) 100))
  (define century-offset
    (match century-code
      [0 1900]
      [2 2000]
      [3 2100]
      [4 2200]
      ;; in the year 2300 we just give up.
      [_ (raise-argument-error 'qtr-year
                               "qtr with century code of 0, 2, 3, or 4"
                               0 qtr)]))
  (+ century-offset year-code))

;; given a year and a season, return the qtr number
(define (encode-qtr [year : Natural] [season : (U Symbol String)]) : Qtr
  (define century-code
    (cond [(<= 1900 year 1999) 0]
          [(<= 2000 year 2099) 2]
          [(<= 2100 year 2199) 3]
          [(<= 2200 year 2299) 4]
          ;; here we just give up
          [else (raise-argument-error 'encode-qtr
                                      "year between 1900 and 2300"
                                      0 year)]))
  (+ (* 1000 century-code)
     (* 10 (modulo year 100))
     (season->qtr-offset (coerce-season season))))

;; given a quarter, return its string form, e.g. 2018 -> "Fall 2001"
(define (qtr->string [qtr : Natural]) : String
  (string-append (qtr->season qtr) " " (number->string (qtr->year qtr))))

;; given a string form, e.g. "Fall 2001", return the corresponding qtr
(define (string->qtr [str : String]) : Natural
  (match str
    [(regexp #px"^([^ ]+) ([0-9]+)$" (list _ season year))
     ;; casts must succeed by definition of regexp
     (encode-qtr (cast (string->number (cast year String)) Natural)
                 (cast season String))]
    [other
     (raise-argument-error 'string->qtr
                           "string like \"Fall 2001\""
                           0 str)]))

;; return the quarter numbers greater than or equal
;; to the first quarter and less than the second.
;; ignore summer quarters.
(: qtrs-in-range (Natural Natural [#:include-summer? Boolean]-> (Listof Natural)))
(define (qtrs-in-range min max #:include-summer? [include-summer? #f])
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
         (define qtr-digits
           (cond [include-summer? '(2 4 6 8)]
                 [else '(2 4 8)]))
         (for/list ([qtr : Natural (in-range min max)]
                    #:when (member (modulo qtr 10) qtr-digits))
           qtr)]))



;; given a year, return the quarters of the academic year beginning in
;; the fall of the given year.
(define (year->qtrs [year : Natural]) : (Listof Qtr)
  (qtrs-in-range (fall-year->base-qtr year)
                 (fall-year->base-qtr (add1 year))))

;; given a catalog cycle, return the quarters that fall into it.
(define (catalog-cycle->qtrs [cycle : CatalogCycle]) : (Listof Qtr)
  (apply append (map year->qtrs (catalog-cycle->fall-years cycle))))

(define (season-after-qtr [season : Season] [qtr : Qtr]) : Qtr
  (define desired-offset (season->qtr-offset season))
  (define qtr-offset (modulo qtr 10))
  (cond [(<= qtr-offset desired-offset) (encode-qtr (qtr->year qtr) season)]
        [else (encode-qtr (add1 (qtr->year qtr)) season)]))

(module+ test
  (require typed/rackunit)


  
  (check-equal? (catalog-cycle->fall-years "1994-1997") '(1994 1995 1996))
  (check-equal? (catalog-cycle->fall-years "2015-2017") '(2015 2016))

  (check-equal? (catalog-cycle->qtrs "2015-2017")
                '(2158 2162 2164 #;2166 2168 2172 2174 #;2176))
  
  (check-equal? (qtr->year 2018) 2001)
  (check-equal? (qtr->year 976) 1997)
  
  (check-equal? (fall-year->base-qtr 2017) 2178)
  (check-equal? (fall-year->base-qtr 1997) 978)
  (check-equal? (qtr->fall-year 2178) 2017)
  (check-equal? (qtr->fall-year 2176) 2017)
  (check-equal? (qtr->fall-year 2174) 2016)
  (check-equal? (qtr->fall-year 788) 1978)
  
  (check-equal? (qtr->catalog-cycle 2178) "2017-2019")
  (check-equal? (qtr->catalog-cycle 964)  "1994-1997")
  
  (check-equal? (fall-year->catalog-cycle 2006) "2005-2007")
  (check-equal? (fall-year->catalog-cycle 2000) "2000-2001")
  (check-equal? (fall-year->catalog-cycle 1999) "1999-2000")

  (check-equal? (qtrs-in-range 2154 2182)
                '(2154 2158 2162 2164 2168 2172 2174 2178))
  (check-equal? (qtrs-in-range 2154 2182 #:include-summer? #t)
                '(2154 2156 2158 2162 2164 2166 2168 2172 2174 2176 2178))
  (check-equal? (qtrs-in-range 982 2014)
                '(982 984 988 992 994 998 2002 2004 2008 2012))

  (check-equal? (qtr->season 2018) "Fall")
  (check-equal? (qtr->season 2102) "Winter")

  (check-equal? (qtr->string 2102) "Winter 2010")
  (check-equal? (qtr->string 328) "Fall 1932")
  (check-equal? (string->qtr "Fall 1932") 328)

  (check-equal? (year->qtrs 2016) '(2168 2172 2174))

  (check-equal? (encode-qtr 2023 "Fall") 2238)
  (check-equal? (encode-qtr 1984 "Winter") 842)
  (check-equal? (encode-qtr 1984 "winter") 842)
  (check-equal? (encode-qtr 1984 'winter) 842)

  (check-equal? (season-after-qtr "Fall" 2188) 2188)
  (check-equal? (season-after-qtr "Spring" 2188) 2194)
  (check-equal? (season-after-qtr "Fall" 2184) 2188)
)