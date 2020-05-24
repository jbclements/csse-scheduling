#lang typed/racket/base

;; functions for mapping back and forth between quarters,
;; fall years, and catalog cycles

;; HONESTLY, the right thing to do here is to establish a bijection between quarters and the integers.
;; this would make it much easier to go to the previous/next quarter, do quarters in range, etc.

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
         next-qtr
         next-qtr/no-summer
         prev-qtr
         prev-qtr/no-summer
         qtr-subtract
         qtr-subtract/no-summer
         qtr-add
         qtr-add/no-summer
         Season)

;; this is the natural encoding of a quarter
(define-type Qtr-Pair (Pairof Natural Season))

(define-type Season (U "Winter" "Spring" "Summer" "Fall"))

(require/typed "qtr-enum.rkt"
               [enum-qtr->n (-> Qtr-Pair Natural)]
               [enum-n->qtr (-> Natural Qtr-Pair)])

(require/typed "qtr-enum-nosummer.rkt"
               [(enum-qtr->n enum-qtr->n/nosmr) (-> Qtr-Pair Natural)]
               [(enum-n->qtr enum-n->qtr/nosmr) (-> Natural Qtr-Pair)])

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
 "2015-2017" "2017-2019" "2019-2020" "2020-2021")

;; make sure each pair lines up, and that each cycle is valid
(define (check-all-cycles [all-cycles : (Listof String)])
  : Void
  (for ([c1 (in-list all-cycles)])
    (check-one-cycle c1))
  (for ([c1 (in-list all-cycles)]
             [c2 (in-list (cdr all-cycles))])
    (match-define (list c1-start c1-finish) (parse-catalog-cycle c1))
    (match-define (list c2-start c2-finish) (parse-catalog-cycle c2))
    (unless (= c1-finish c2-start)
      (error 'cycle-check
             "end of cycle ~v does not match beginning of cycle ~v"
             c1 c2))))

;; check one cycle for validity
(define (check-one-cycle [c1 : String])
  (match-define (list c1-start c1-finish) (parse-catalog-cycle c1))
    (unless (< c1-start c1-finish)
      (error 'cycle-check
             "cycle ~v ends on or before it starts"
             c1)))

;; given a catalog-cycle of the form "2019-2020", return (list 2019 2020)
(define (parse-catalog-cycle [cycle : String]) : (List Natural Natural)
  (match cycle
    [(regexp "^([0-9]+)-([0-9]+)$" (list _ a b))
     ;; casts must succeed by definition of regexp:
     (list (cast (string->number (cast a String)) Natural)
           (cast (string->number (cast b String)) Natural))]
    [other (error 'parse-catalog-cycle "cycle does not match expected format: ~v"
                  cycle)]))


(check-all-cycles all-cycles)



(define-predicate catalog-cycle? CatalogCycle)

;; given a year (e.g. 2015), return the catalog cycle that
;; fall of that year falls into (in this case, "2015-2017").
(define (fall-year->catalog-cycle [year : Natural]) : CatalogCycle
  (define maybe-match
    (findf (λ ([cycle : String])
             (match-define (list start end) (parse-catalog-cycle cycle))
             (and (<= start year)
                  (< year end)))
           all-cycles))
  (match maybe-match
    [#f (raise-argument-error
         'fall-year->catalog-cycle
         "year mapping to known cycle (extend list?)"
         0 year)]
    ;; cast must succeed by definition of cycle-defs
    [s (cast s CatalogCycle)]))

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

;; given a year and a season, return the cal poly qtr number
(define (encode-qtr [year : Natural] [season : (U Symbol String)]) : Qtr
  (encode-qtr/2 (cons year (coerce-season season))))

;; given qtr-pair, return the cal poly qtr number
(define (encode-qtr/2 [qp : Qtr-Pair]) : Qtr
  (define year (car qp))
  (define season (cdr qp))
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

;; map a cal poly qtr number to a qtr-pair
(define (decode-qtr [qtr : Qtr]) : Qtr-Pair
  (cons (qtr->year qtr) (qtr->season qtr)))

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
  (define qtr-pairs
    (map enum-n->qtr
         (range (enum-qtr->n (decode-qtr min))
                (enum-qtr->n (decode-qtr max)))))
  (define filtered-pairs
    (filter (cond [include-summer? (λ (x) x)]
                  [else (λ ([qpr : Qtr-Pair]) (not (equal? (cdr qpr) "Summer")))])
            qtr-pairs))
  (map encode-qtr/2 filtered-pairs))

;; given a year, return the quarters of the academic year beginning in
;; the fall of the given year.
(define (year->qtrs [year : Natural]) : (Listof Qtr)
  (qtrs-in-range (fall-year->base-qtr year)
                 (fall-year->base-qtr (add1 year))))

;; given a catalog cycle, return the quarters that fall into it.
(define (catalog-cycle->qtrs [cycle : CatalogCycle]) : (Listof Qtr)
  (apply append (map year->qtrs (catalog-cycle->fall-years cycle))))

;; return the cal poly number of the first quarter following 'qtr'
;; that has the season 'season'.
(define (season-after-qtr [season : Season] [qtr : Qtr]) : Qtr
  (define qtr-pair-n : Natural (enum-qtr->n (decode-qtr qtr)))
  ;; a nice stream of following quarters would be easier to read...
  (define matching-n
    (let loop : Natural [(i : Natural 0)]
      (cond [(equal? season (cdr (enum-n->qtr (+ qtr-pair-n i))))
             (+ qtr-pair-n i)]
            [else (loop (add1 i))])))
  (encode-qtr/2 (enum-n->qtr matching-n)))

(define (qtr->n [qtr : Qtr]) : Natural
  (enum-qtr->n (decode-qtr qtr)))

(define (qtr->n/nosmr [qtr : Qtr]) : Natural
  (enum-qtr->n/nosmr (decode-qtr qtr)))

(define (n->qtr [i : Integer]) : Qtr
  (encode-qtr/2 (enum-n->qtr (ensure-natural-idx i))))

(define (n->qtr/nosmr [i : Integer]) : Qtr
  (encode-qtr/2 (enum-n->qtr/nosmr (ensure-natural-idx i))))

;; how many qtrs between these two? (1 = next quarter)
(define (qtr-subtract [qtrA : Qtr] [qtrB : Qtr]) : Integer
  (- (qtr->n qtrA)
     (qtr->n qtrB)))

;; how many qtrs between these two, omitting all summers?
(define (qtr-subtract/no-summer [qtrA : Qtr] [qtrB : Qtr]) : Integer
  (- (qtr->n/nosmr qtrA)
     (qtr->n/nosmr qtrB)))

;; what's the quarter 'n' quarters later? (1 = next quarter, -1 = last quarter)
(define (qtr-add [qtrA : Qtr] [i : Integer]) : Qtr
  (n->qtr (+ i (qtr->n qtrA))))

;; same but ignore summer.
(define (qtr-add/no-summer [qtrA : Qtr] [i : Integer]) : Qtr
  (n->qtr/nosmr (+ i (qtr->n/nosmr qtrA))))

;; what quarter comes after this one?
(define (next-qtr [qtr : Qtr]) : Qtr
  (qtr-add qtr 1))

;; ignoring summer, what quarter comes after this one?
(define (next-qtr/no-summer [qtr : Qtr]) : Qtr
  (qtr-add/no-summer qtr 1))

;; this will get triggered for the year zero...
(define (ensure-natural-idx [n : Integer]) : Natural
  (cond [(< n 0) (error 'ensure-natural-idx "expected number >= 0, got ~v" n)]
        [else n]))

;; return the qtr before this one
(define (prev-qtr [qtr : Qtr]) : Qtr
  (qtr-add qtr -1))

(define (prev-qtr/no-summer [qtr : Qtr]) : Qtr
  (qtr-add/no-summer qtr -1))

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
  (check-equal? (fall-year->catalog-cycle 2019) "2019-2020")

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

  (check-equal? (parse-catalog-cycle "2019-2020") (list 2019 2020))
  (check-exn #px"does not match beginning"
             (λ () (check-all-cycles (list "123-125" "127-129"))))
  (check-exn #px"ends on or before it starts"
             (λ () (check-all-cycles (list "2019-2019"))))
  (check-exn #px"does not match expected format"
             (λ () (check-all-cycles (list "1234"))))

  (check-equal? (next-qtr 2148) 2152)
  (check-equal? (next-qtr 2144) 2146)
  (check-equal? (next-qtr/no-summer 2144) 2148)

  (check-equal? (prev-qtr 2152) 2148)
  (check-equal? (prev-qtr 2148) 2146)
  (check-equal? (prev-qtr/no-summer 2148) 2144)
  (check-equal? (qtr-add 2148 -1) 2146)
  (check-equal? (qtr-add 2148 -2) 2144)
  (check-equal? (qtr-add/no-summer 2148 -2) 2142)

  (check-equal? (qtr-subtract 2204 2196) 3)
  (check-equal? (qtr-subtract 2202 2204) -1)

  (check-equal? (qtr-subtract 2204 2194) 4)
  (check-equal? (qtr-subtract/no-summer 2204 2194) 3)
  
)