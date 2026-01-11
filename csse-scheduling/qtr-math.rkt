#lang typed/racket/base

;; functions for mapping back and forth between quarters,
;; fall years, and catalog cycles

;; there are four different representations of a quarter:
;; 1) A pair of season and year.
;; 2) A cal poly quarter number (represented as a natural) -- not sequential!
;; 3) A sequential numbering of all quarters including summer quarter
;; 4) A sequential numbering of all quarters not including summer quarter.
;; This code can map between any of them. It does lost of other stuff too.

(require racket/match
         "./types.rkt"
         (only-in racket/list range))

(provide CatalogCycle
         catalog-cycle?
         fall-year->catalog-cycle
         catalog-cycle->fall-years
         catalog-cycle-<?
         fall-year->base-qtr
         qtr->fall-year
         qtr->catalog-cycle
         qtrs-in-range
         qtr->season
         qtr->year
         qtr->string
         string->qtr
         fall-year->qtrs
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
         qtrs-per-year/no-summer
         Season
         Qtr

         fall-year->base-term
         term->fall-year ;; check
         term->catalog-cycle ;; check
         terms-in-range ;; check
         term->season ;; checked
         term->year ;; check
         term->string ;; check
         string->term
         fall-year->terms
         catalog-cycle->terms
         encode-term ;; check?
         season-after-term ;; check
         next-term ;; check
         next-term/no-summer ;; check
         prev-term ;; check
         prev-term/no-summer ;; check
         term-subtract ;; check
         term-subtract/no-summer ;; check
         term-add ;; check
         term-add/no-summer ;; check
         sems-per-year/no-summer
         CPTN

         coerce-season
         semester-transition-year
         first-semester-year ;; same as semester-transition-year, this is a better name...
         first-semester-cptn
         semester-term?
         semester-catalog?
         )

;; this is the natural encoding of a quarter
(define-type Term-Pair (Pairof Natural Season))

(define-type Season (U "Winter" "Spring" "Summer" "Fall"))

(define seasons-count 4)

(require/typed "term-enum.rkt"
               [enum-term->n (-> Term-Pair Natural)]
               [enum-n->term (-> Natural Term-Pair)]
               [enum-term->n/no-smr (-> Term-Pair Natural)]
               [enum-n->term/no-smr (-> Natural Term-Pair)]
               [term? (-> Term-Pair Boolean)]
               ;; not a great name, should have used first-semester-year
               [semester-transition-year Natural]
               [first-semester-year Natural]
               )

(define first-encodable-year : Natural 1900)

;; this is a little D.R.Y. macro to avoid listing
;; the names of the cycles twice.`
(define-syntax cycle-defs
  (syntax-rules ()
    [(_ type-name list-name cycle ...)
     (begin
       (define-type type-name (U cycle ...))
       (define list-name (list cycle ...)))]))

;; extend as needed... Also, add the new year to the configuration-fetcher
;; in fetch-mappings.rkt
(cycle-defs
 CatalogCycle
 all-cycles
 "1994-1997" "1997-1998" "1998-1999" "1999-2000" "2000-2001"
 "2001-2003" "2003-2005"
 "2005-2007" "2007-2009" "2009-2011" "2011-2013" "2013-2015"
 "2015-2017" "2017-2019" "2019-2020" "2020-2021" "2021-2022" "2022-2026"
 ;; first semester catalog:
 "2026-2028")

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
;; map to it, in numerical order
(define (catalog-cycle->fall-years [cycle : CatalogCycle]) : (Listof Natural)
  (match cycle
    [(regexp #px"^(\\d{4})-(\\d{4})$" (list _ startstr endstr))
     (define start-year (assert (string->number (assert startstr string?)) natural?))
     (define end-year (assert (string->number (assert endstr string?)) natural?))
     (range start-year end-year)]))

;; given two cycles, return true if the first one occurs before the last one.
;; assumes that catalog cycles don't overlap or skip years...
(define (catalog-cycle-<? [a : CatalogCycle] [b : CatalogCycle])
  (< (car (catalog-cycle->fall-years a))
     (car (catalog-cycle->fall-years b))))

(define natural? exact-nonnegative-integer?)

;; map a year to the fall cptn that occurs in it (see example below)
(define (fall-year->base-term [year : Natural]) : CPTN
  (encode-term year "Fall"))
(define fall-year->base-qtr fall-year->base-term) ;; bridge

;; map a year to the summer cptn that occurs in it. (*before* the fall quarter.)
(define (fall-year->summer-term [year : Natural]) : CPTN
  (encode-term year "Summer"))

;; map a qtr to the fall year (summer goes forward...)
(define (term->fall-year [term : CPTN]) : Natural
  (define base-year (term->year term)) 
  ;; winter/spring go backward
  (match (term->season term)
    [(or "Winter" "Spring")
     (define year (- base-year 1))
     (cond [(<= first-encodable-year year) year]
           [else (raise-argument-error 'term->fall-year
                                       "qtr whose fall year is encodable"
                                       0 term)])]
    ;; Fall/Summer stay the same.
    [_ base-year]))
(define qtr->fall-year term->fall-year) ;; bridge

;; map a qtr to the cycle that it occurs in
(define (term->catalog-cycle [term : CPTN]) : CatalogCycle
  (fall-year->catalog-cycle (term->fall-year term)))
(define qtr->catalog-cycle term->catalog-cycle)

;; given a string or symbol, coerce it to a season if possible
(define (coerce-season [s : (U Symbol String)]) : Season
  (match s
    [(or 'w 'winter "winter" "Winter") "Winter"]
    [(or 's 'spring "spring" "Spring") "Spring"]
    [(or 'summer "summer" "Summer") "Summer"]
    [(or 'f 'fall "fall" "Fall") "Fall"]
    [other
     (raise-argument-error 'coerce-season
                           "season-like symbol or string"
                           0 s)]))

;; given a quarter, return its season: "Fall", "Winter", etc.
(define (term->season [term : CPTN]) : Season
  (legal-cptn-check term 'term->season)
  (term->season/unchecked term))

;; don't check whether this is a legal season (required to avoid infinite loop in cptn? )
(define (term->season/unchecked [term : CPTN]) : Season
  (match (modulo term 10)
    [2 "Winter"]
    [4 "Spring"]
    [6 "Summer"]
    [8 "Fall"]
    [other (error 'qtr->season "not a legal last quarter digit: ~e" other)]))
(define qtr->season term->season)

;; given a season, return its quarter offset
(define (season->term-offset [season : Season]) : Natural
  (match season
    ["Winter" 2]
    ["Spring" 4]
    ["Summer" 6]
    ["Fall" 8]
    [other (error 'season->qtr-offset "not a legal season: ~v\n" other)]))
(define season->qtr-offset season->term-offset)

;; FIXME
;; okay this one is actually scary:
(define qtrs-per-year/no-summer 3)
(define sems-per-year/no-summer 2)

;; return the year in which a CPTN falls
(define (term->year [term : CPTN]) : Natural
  (legal-cptn-check term 'term->year)
  (term->year/unchecked term))

(define (term->year/unchecked [term : CPTN]) : Natural
  (define century-code (floor (/ term 1000)))
  (define year-code (modulo (floor (/ term 10)) 100))
  (define century-offset
    (match century-code
      [0 1900]
      [2 2000]
      [3 2100]
      [4 2200]
      ;; in the year 2300 we just give up.
      [_ (raise-argument-error 'qtr-year
                               "qtr with century code of 0, 2, 3, or 4"
                               0 term)]))
  (+ century-offset year-code))
(define qtr->year term->year) ;; bridge

;; given a year and a season, return the cal poly qtr number
(define (encode-term [year : Natural] [season : (U Symbol String)]) : CPTN
  (encode-term/2 (cons year (coerce-season season))))
(define encode-qtr encode-term) ;; bridge

;; given term-pair, return the cal poly qtr number
(define (encode-term/2 [tp : Term-Pair]) : CPTN
  (legal-term-pair-check tp 'encode-term/2)
  (define year (car tp))
  (define season (cdr tp))
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
     (season->term-offset (coerce-season season))))



;; map a cal poly qtr number to a qtr-pair
(define (decode-term [term : CPTN]) : Term-Pair
  (define result (cons (term->year term) (term->season term)))
  (unless (term? result)
    (error 'decode-term "expected legal term number, got: ~e" term))
  result)

(define (cptn? [term : CPTN]) : Boolean
  (term? (cons (term->year/unchecked term) (term->season/unchecked term))))

;; given a term, return its string form, e.g. 2018 -> "Fall 2001"
(define (term->string [term : CPTN]) : String
  (string-append (term->season term) " " (number->string (term->year term))))
(define qtr->string term->string) ;; bridge

;; given a string form, e.g. "Fall 2001", return the corresponding term number
(define (string->term [str : String]) : CPTN
  (match str
    [(regexp #px"^([^ ]+) ([0-9]+)$" (list _ season year))
     ;; casts must succeed by definition of regexp
     (encode-term (cast (string->number (cast year String)) Natural)
                  (cast season String))]
    [other
     (raise-argument-error 'string->qtr
                           "string like \"Fall 2001\""
                           0 str)]))
(define string->qtr string->term) ;; bridge


;; return the quarter numbers greater than or equal
;; to the first quarter and less than the second.
;; ignore summer quarters.
(define (terms-in-range [min : CPTN] [max : CPTN] #:include-summer? [include-summer? : Boolean #f])
  : (Listof CPTN)
  (define term-pairs
    (map enum-n->term
         (range (enum-term->n (decode-term min))
                (enum-term->n (decode-term max)))))
  (define filtered-pairs
    (filter (cond [include-summer? (λ (x) x)]
                  [else (λ ([tpr : Term-Pair]) (not (equal? (cdr tpr) "Summer")))])
            term-pairs))
  (map encode-term/2 filtered-pairs))
(define qtrs-in-range terms-in-range) ;; bridge

;; given a year, return the quarters of the academic year beginning in
;; the fall of the given year (omitting all summer quarters)
(define (fall-year->terms [year : Natural]) : (Listof CPTN)
  (terms-in-range (fall-year->base-term year)
                  (fall-year->base-term (add1 year))))
(define fall-year->qtrs fall-year->terms) ;; bridge

;; given a year, return the quarters of the academic year beginning in
;; the summer of the given year.
(define (fall-year->terms/summer [year : Natural]) : (Listof CPTN)
  (terms-in-range (fall-year->summer-term year)
                  (fall-year->summer-term (add1 year))
                  #:include-summer? #t))

;; given a catalog cycle, return the quarters that fall into it.
(define (catalog-cycle->terms [cycle : CatalogCycle]
                              #:include-summer? [include-summer? : Boolean #f])
  : (Listof CPTN)
  (define enumerator (cond [include-summer? fall-year->terms/summer]
                           [else fall-year->terms]))
  (apply append (map enumerator (catalog-cycle->fall-years cycle))))
(define catalog-cycle->qtrs catalog-cycle->terms) ;; bridge

;; return the cal poly number of the first term following 'term'
;; that has the season 'season'.
(define (season-after-term [season : Season] [term : CPTN]) : CPTN
  (define qtr-pair-n : Natural (enum-term->n (decode-term term)))
  ;; a nice stream of following quarters would be easier to read...
  (define matching-n
    (let loop : Natural [(i : Natural qtr-pair-n)
                         (count : Natural 0)]
      (cond [(equal? season (cdr (enum-n->term i))) i]
            ;; give up after two years (winter never comes again after 2262)
            [(>= count (* 2 seasons-count))
             (error 'season-after-term
                    "can't find any more of season ~e after ~e"
                    season term)]
            [else (loop (add1 i) (add1 count))])))
  (encode-term/2 (enum-n->term matching-n)))
(define season-after-qtr season-after-term) ;; bridge

(define (term->n [term : CPTN]) : Natural
  (enum-term->n (decode-term term)))

(define (term->n/no-smr [term : CPTN]) : Natural
  (enum-term->n/no-smr (decode-term term)))

(define (n->term [i : Integer]) : CPTN
  (encode-term/2 (enum-n->term (ensure-natural-idx i))))

(define (n->term/no-smr [i : Integer]) : CPTN
  (encode-term/2 (enum-n->term/no-smr (ensure-natural-idx i))))

;; how many qtrs between these two? (1 = next quarter)
(define (term-subtract [termA : CPTN] [termB : CPTN]) : Integer
  (- (term->n termA)
     (term->n termB)))
(define qtr-subtract term-subtract) ;; bridge

;; how many qtrs between these two, omitting all summers?
(define (term-subtract/no-summer [termA : CPTN] [termB : CPTN]) : Integer
  (- (term->n/no-smr termA)
     (term->n/no-smr termB)))
(define qtr-subtract/no-summer term-subtract/no-summer) ;; bridge

;; what's the quarter 'n' quarters later? (1 = next quarter, -1 = last quarter)
(define (term-add [termA : CPTN] [i : Integer]) : CPTN
  (n->term (+ i (term->n termA))))
(define qtr-add term-add) ;; bridge

;; same but ignore summer.
(define (term-add/no-summer [termA : CPTN] [i : Integer]) : CPTN
  (n->term/no-smr (+ i (term->n/no-smr termA))))
(define qtr-add/no-summer term-add/no-summer) ;; bridge

;; what quarter comes after this one?
(define (next-term [term : CPTN]) : CPTN
  (term-add term 1))
(define next-qtr next-term) ;; bridge

;; ignoring summer, what quarter comes after this one?
(define (next-term/no-summer [term : CPTN]) : CPTN
  (term-add/no-summer term 1))
(define next-qtr/no-summer next-term/no-summer) ;; bridge

;; this will get triggered for the year zero...
(define (ensure-natural-idx [n : Integer]) : Natural
  (cond [(< n 0) (error 'ensure-natural-idx "expected number >= 0, got ~v" n)]
        [else n]))


;; what term comes before this one?
(define (prev-term [term : CPTN]) : CPTN
  (term-add term -1))
(define prev-qtr prev-term) ;; bridge

;; ignoring summer, what term comes after this one?
(define (prev-term/no-summer [term : CPTN]) : CPTN
  (term-add/no-summer term -1))
(define prev-qtr/no-summer prev-term/no-summer) ;; bridge


(define (legal-cptn-check [term : CPTN] [fun-id : Symbol]) : True
  (unless (cptn? term)
    (error fun-id "expected legal term number, got: ~e" term))
  #t)

(define (legal-term-pair-check [tp : Term-Pair] [fun-id : Symbol]) : True
  (when (not (term? tp))
    (error fun-id "expected legal term pair, got: ~e" tp))
  #t)

;; honestly this is should be the "base" specification
(define first-semester-catalog-cycle : CatalogCycle "2026-2028")


;; the first term that is a semester
(define first-semester-cptn (encode-term semester-transition-year "Fall"))

;; is the given catalogy cycle a semester catalog cycle?
(define (semester-catalog? [cc : CatalogCycle])
  (<= first-semester-cptn (car (catalog-cycle->terms cc))))

(unless (equal? (car (catalog-cycle->terms first-semester-catalog-cycle))
                first-semester-cptn)
  (error 'first-semester-cptn
         "first semester CPTN ~a should be the same as first term in first semester catalog ~a"
         first-semester-cptn
         (car (catalog-cycle->terms first-semester-catalog-cycle))))

(define (semester-term? [term : CPTN])
  (<= first-semester-cptn term))

(module+ test
  (require typed/rackunit
           syntax/parse
           (for-syntax syntax/parse
                       racket))


  
  (check-equal? (catalog-cycle->fall-years "1994-1997") '(1994 1995 1996))
  (check-equal? (catalog-cycle->fall-years "2015-2017") '(2015 2016))

  (check-equal? (catalog-cycle->qtrs "2015-2017")
                '(2158 2162 2164 #;2166 2168 2172 2174 #;2176))
  (check-equal? (catalog-cycle->qtrs (ann "2015-2017" CatalogCycle) #:include-summer? #t)
                '(2156 2158 2162 2164 2166 2168 2172 2174))

  (check-equal? (catalog-cycle->terms "2015-2017")
                '(2158 2162 2164 #;2166 2168 2172 2174 #;2176))
  (check-equal? (catalog-cycle->terms (ann "2015-2017" CatalogCycle) #:include-summer? #t)
                '(2156 2158 2162 2164 2166 2168 2172 2174))

  (check-equal? (catalog-cycle->qtrs "2026-2028")
                '(#;2266 2268 2274 #;2276 2278 2284))
  (check-equal? (catalog-cycle->qtrs (ann "2026-2028" CatalogCycle) #:include-summer? #t)
                '(2266 2268 2274 2276 2278 2284))
  
  (check-equal? (qtr->year 2018) 2001)
  (check-equal? (qtr->year 976) 1997)

  (check-equal? (term->year 2284) 2028)
  (check-equal? (term->year 2018) 2001)
  (check-equal? (term->year 976) 1997)
  
  (check-equal? (fall-year->base-qtr 2017) 2178)
  (check-equal? (fall-year->base-qtr 1997) 978)
  (check-equal? (qtr->fall-year 2178) 2017)
  (check-equal? (qtr->fall-year 2176) 2017)
  (check-equal? (qtr->fall-year 2174) 2016)
  (check-equal? (qtr->fall-year 788) 1978)

  (check-equal? (fall-year->base-term 2028) 2288)
  (check-equal? (fall-year->base-term 2017) 2178)
  (check-equal? (fall-year->base-term 1997) 978)
  (check-equal? (term->fall-year 2288) 2028)
  (check-equal? (term->fall-year 2284) 2027)
  (check-equal? (term->fall-year 2178) 2017)
  (check-equal? (term->fall-year 2176) 2017)
  (check-equal? (term->fall-year 2174) 2016)
  (check-equal? (term->fall-year 788) 1978)
  
  (check-equal? (qtr->catalog-cycle 2178) "2017-2019")
  (check-equal? (qtr->catalog-cycle 964)  "1994-1997")

  (check-equal? (term->catalog-cycle 2284) "2026-2028")
  (check-equal? (term->catalog-cycle 2178) "2017-2019")
  (check-equal? (term->catalog-cycle 964)  "1994-1997")
  
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

  (check-equal? (terms-in-range 2154 2182)
                '(2154 2158 2162 2164 2168 2172 2174 2178))
  (check-equal? (terms-in-range 2154 2182 #:include-summer? #t)
                '(2154 2156 2158 2162 2164 2166 2168 2172 2174 2176 2178))
  (check-equal? (terms-in-range 982 2014)
                '(982 984 988 992 994 998 2002 2004 2008 2012))
  (check-equal? (terms-in-range 2264 2278) '(2264 #;2266 2268 2274 #;2276))
  (check-equal? (terms-in-range 2264 2278 #:include-summer? #t)
                '(2264 2266 2268 2274 2276))

  (check-equal? (qtr->season 2018) "Fall")
  (check-equal? (qtr->season 2102) "Winter")

  (check-equal? (term->season 2018) "Fall")
  (check-equal? (term->season 2102) "Winter")  
  (check-equal? (term->season 2274) "Spring")

  (check-equal? (qtr->string 2102) "Winter 2010")
  (check-equal? (qtr->string 328) "Fall 1932")
  (check-equal? (string->qtr "Fall 1932") 328)

  (check-equal? (term->string 2102) "Winter 2010")
  (check-equal? (term->string 328) "Fall 1932")
  (check-equal? (term->string 2288) "Fall 2028")
  
  (check-equal? (string->term "Fall 1932") 328)

  (check-equal? (fall-year->qtrs 2016) '(2168 2172 2174))

  (check-equal? (encode-qtr 2023 "Fall") 2238)
  (check-equal? (encode-qtr 1984 "Winter") 842)
  (check-equal? (encode-qtr 1984 "winter") 842)
  (check-equal? (encode-qtr 1984 'winter) 842)

  (check-equal? (season-after-qtr "Fall" 2188) 2188)
  (check-equal? (season-after-qtr "Spring" 2188) 2194)
  (check-equal? (season-after-qtr "Fall" 2184) 2188)
  (check-equal? (season-after-qtr "Winter" 2184) 2192)
  (check-exn #px"can't find any more of season"
             (λ ()(season-after-qtr "Winter" 2268)))

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

  (check-equal? (catalog-cycle-<? "2005-2007" "2020-2021") #t)
  (check-equal? (catalog-cycle-<? "2019-2020" "2019-2020") #f)
  (check-equal? (catalog-cycle-<? "2020-2021" "2019-2020") #f)

  #;(define-syntax (mylet stx)
    (syntax-parse stx
      [(_ ((var:id rhs:expr) ...) body ...+)
       #'((lambda (var ...) body ...) rhs ...)]))

  (define-syntax (legal-term-check stx)
    (syntax-parse stx
      [(_ fun)
       (syntax/loc stx
         (check-exn #px"legal term"
                    (λ () (fun 2282))))]))

  (check-equal? (fall-year->terms 2025) '(2258 2262 2264))
  (check-equal? (fall-year->terms 2026) '(2268 2274))

  (legal-term-check term->fall-year)
  (legal-term-check term->catalog-cycle)
  ;(legal-term-check terms-in-range)
  (legal-term-check term->season)
  (legal-term-check term->year)
  (legal-term-check term->string)
  (legal-term-check next-term)
  (legal-term-check next-term/no-summer)
  (legal-term-check prev-term)
  (legal-term-check prev-term/no-summer)

  (define lt #px"legal term")
  (check-exn lt (λ () (term-subtract 2282 4)))
  (check-exn lt (λ () (term-subtract/no-summer 2282 4)))
  (check-exn lt (λ () (term-add 2282 4)))
  (check-exn lt (λ () (term-add/no-summer 2282 4)))
  
  (legal-term-check term->season)
  (legal-term-check term->string)

  (check-equal? (semester-catalog? (ann "2022-2026" CatalogCycle)) #f)
  (check-equal? (semester-catalog? (ann "2026-2028" CatalogCycle)) #t)
  ;; (season-after-qtr "Winter" 2268)
)