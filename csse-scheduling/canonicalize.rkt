#lang typed/racket/base

;; this file maps subject/number pairs to canonical ids.
;; at this point, it's sort of turning into a thin wrapper
;; for the scheduling database.

(provide canonicalize
         canonicalize/qtr
         canonicalize/noerr
         canonicalize/qtr/noerr
         canonicalize/num
         canonical-id?
         ensure-canonical
         courses-in-subject
         coursenum?
         course-key
         id-mappings
         id->mapping
         Subject
         CourseNum
         Course-Id
         subject?
         mappings
         MappingRow
         CatalogCycle
         last-mapped-qtr
         default-subject-preference-order
         id-has-subj-mapping?
         )

(require/typed "fetch-mapping.rkt"
               [course-mappings Any])

(require racket/match
         racket/set
         "types.rkt"
         "qtr-math.rkt"
         (only-in racket/list remove-duplicates filter-map)
         threading)

;; if any of these are not in #px"^[A-Z]+$", serious collision problems could occur
(define-type Subject
  (U "AEPS" "AERO" "AG" "AGB" "AGC" "ANT" "ARCE" "ARCH" "ART" "ASCI" "ASTR" "BIO"
     "BMED" "BOT" "BRAE" "BUS" "CD" "CE" "CHEM" "CHIN" "CM" "COM" "COMS" "CPE" "CRP"
     "CSC" "DANC" "DATA" "DSCI" "ECON" "EDES" "EDUC" "EE" "ENGL" "ENGR" "ENVE" "ERSC"
     "ES" "ESE" "ESM" "FPE" "FR" "FSN" "GEOG" "GEOL" "GER" "GRC" "GS" "GSA" "GSB" "GSE" "HCS" "HIST" "HLTH" "HNRC"
     "HNRS" "HUM" "IME" "ISLA" "IT" "ITAL" "ITP" "JOUR" "JPNS" "KINE" "LA"
     "LAES" "LS" "MATE" "MATH" "MCRO" "ME" "MLL" "MSCI" "MSL" "MU" "NR" "PEM" "PEW"
     "PHIL" "PHYS" "PLSC" "POLS" "PSC" "PSY" "RELS" "RPTA" "SCM" "SOC" "SPAN" "SS"
     "STAT" "TH" "UNIV" "WGS" "WGQS" "WLC" "WVIT" "ZOO"))

;; if any of these don't start with a digit, serious problems could occur
(define-type CourseNum String) ;; such as "123" or "0123"

(define-type MappingRow (Vector CatalogCycle Subject CourseNum Course-Id))

(define-predicate subject? Subject)

(: mapping-row-id (MappingRow -> Course-Id))
(define (mapping-row-id r)
  (vector-ref r 3))

(: mapping-row-cycle (MappingRow -> CatalogCycle))
(define (mapping-row-cycle r)
  (vector-ref r 0))

(: mapping-row-subject (MappingRow -> Subject))
(define (mapping-row-subject r)
  (vector-ref r 1))

(: mapping-row-course-num (MappingRow -> CourseNum))
(define (mapping-row-course-num r)
  (vector-ref r 2))

;; this can't be captured by a TR type
(: coursenum? (String -> Boolean))
(define (coursenum? s)
  (and (string? s)
       ;; turns out there are two-digit course numbers.
       ;; 2025-09-25 this is kind of scary, I'd like the numbers to start with
       ;; numbers so that they can definitely be separated from subjects for
       ;; the construction of canonical ids:
       (regexp-match? #px"^[0PS]?[0-9]{2,4}[AL]?( +X)?$" s)))

;; the rows in the table, representing mappings from offering
;; to canonical name
(define mappings : (Listof MappingRow)
  (cast course-mappings (Listof MappingRow)))

;; the last catalog cycle for which we have mappings
(define last-mapped-qtr : Qtr
  (apply max
         (apply append
                (map (λ ([r : MappingRow])
                       (catalog-cycle->qtrs (vector-ref r 0)))
                     mappings))))

;; a hash table to speed up the mapping, and also to check that the
;; rows that we got from the db conform to the contracts that we
;; specifiye
(define mapping-hash
  (for/hash : (Immutable-HashTable (Vector CatalogCycle Subject CourseNum)
                                   Course-Id)
    ([v (in-list mappings)])
    (match-define (vector cycle subject coursenum name) v)
    (unless (coursenum? coursenum)
      (error "unexpected values in db row ~e" v))
    (values (vector cycle subject coursenum) name)))



;; given catalog cycle, subject, and number, return a string
;; representing the canonical course name
(: canonicalize (CatalogCycle (U Symbol String) (U Natural CourseNum) -> Course-Id))
(define (canonicalize cycle subject-input number-input)
  (define try-canonicalize (canonicalize/noerr cycle subject-input number-input))
  (cond [(not try-canonicalize)
         (error 'canonicalize
                "no mapping found for offering: ~e"
                (list cycle subject-input number-input))]
        [else try-canonicalize]))


;; like canonicalize, but accepts qtr rather than catalog cycle
(: canonicalize/qtr (Natural (U Symbol String) (U Natural String) -> Course-Id))
(define (canonicalize/qtr qtr subject number)
  ;; SEMESTER FIXME this should be deprecated...
  (canonicalize (qtr->catalog-cycle qtr) subject number))

;; should have named this term, sigh...
(: canonicalize/term (Natural (U Symbol String) (U Natural String) -> Course-Id))
(define (canonicalize/term term subject number)
  (canonicalize (qtr->catalog-cycle term) subject number))


;; like canonicalize, but accepts qtr rather than catalog cycle
(: canonicalize/qtr/noerr (Natural (U Symbol String) (U Natural String) -> (U False Course-Id)))
(define (canonicalize/qtr/noerr qtr subject number)
  (canonicalize/noerr (qtr->catalog-cycle qtr) subject number))

;; this one returns #f when it can't find a mapping
(: canonicalize/noerr (CatalogCycle (U Symbol String) (U Natural CourseNum) -> (U Course-Id False)))
(define (canonicalize/noerr cycle subject-input number-input)
  (define (check-subject [maybe-subject : String]) : Subject
    (cond [(subject? maybe-subject) maybe-subject]
          [else (raise-argument-error 'canonicalize
                                      "string or symbol identifying a subject"
                                      1 cycle subject-input number-input)]))
  (define subject : Subject
    (cond [(symbol? subject-input)
           (check-subject (symbol->string subject-input))]
          [(string? subject-input)
           (check-subject subject-input)]))
  (define (check-num [num : CourseNum]) : CourseNum
    (cond [(coursenum? num) num]
          [else (raise-argument-error 'canonicalize
                                      "legal course number"
                                      2 cycle subject-input number-input)]))
  (define number : CourseNum
    (cond [(exact-nonnegative-integer? number-input)
           (check-num (number->string number-input))]
          [(string? number-input)
           (check-num number-input)]))
  (define trimmed-number
    (match number
      [(regexp #px"^[0PS][0-9]{3}" (list _))
       (substring number 1)]
      [other number]))
  (hash-ref mapping-hash (vector cycle subject trimmed-number) #f))

;; using the CPE and CSC subjects, try canonicalizing a number.
(: canonicalize/num (CatalogCycle (U Natural CourseNum) -> Course-Id))
(define (canonicalize/num cycle number-input)
  ;; we want to allow numbers like 430 to be used without prefixes, but
  ;; only when there's no conflict:
  (define no-prefix-subjects '(CSC CPE DATA))
  (define possibilities
    (remove-duplicates
     (filter-map (λ ([prefix : Symbol]) (canonicalize/noerr cycle prefix number-input))
                 no-prefix-subjects)))
  (cond [(null? possibilities)
         (error 'canonicalize/num
                "no matches for number ~v across these subjects: ~e"
                number-input no-prefix-subjects)]
        [(null? (cdr possibilities))
         (car possibilities)]
        [else ;; too many matches
         (error 'canonicalize/num
           "different matches for number ~v in different subjects: ~e"
           number-input possibilities)]))



;; is this string the canonical id of some course?
(define (canonical-id? [n : Course-Id]) : Boolean
  (set-member? canonical-ids n))

(define canonical-ids : (Setof Course-Id)
  (list->set (hash-values mapping-hash)))

(define (row-name [r : MappingRow]) : String
  (vector-ref r 3))

(define (row-num [r : MappingRow]) : String
  (vector-ref r 2))

;; ensure that a course id is in the canonical list. Signal an error
;; if not. return it if it's okay
;; (this defn must be early to allow its use in courses-we-schedule)
(define (ensure-canonical [s : String]) : String
  (cond [(canonical-id? s) s]
        [else (raise-argument-error
               'ensure-canonical
               "canonical course id"
               0 s)]))



;; return the canonical names of all classes in the given catalog offered
;; with the given subject
(: courses-in-subject (CatalogCycle Subject -> (Listof Course-Id)))
(define (courses-in-subject cycle subject)
  (remove-duplicates
   (map row-name
        (filter (λ ([r : MappingRow])
                  (and (equal? (vector-ref r 0) cycle)
                       (equal? (vector-ref r 1) subject)))
                mappings))))


;;> shouldn't this function take a catalog cycle? Do you ever want all of them across
;;> catalog cycles?

;; return all of the mappings for a given class id
(: id-mappings (Course-Id -> (Listof (List CatalogCycle Subject CourseNum))))
(define (id-mappings id)
  (ensure-canonical id)
  (define rows
    (filter (λ ([r : MappingRow])
              (equal? (mapping-row-id r) id))
            mappings))
  (map (λ ([r : MappingRow])
         (list (vector-ref r 0)
               (vector-ref r 1)
               (vector-ref r 2)))
       rows))

(define (id-mappings/cc [id : Course-Id] [cc : CatalogCycle])
  : (Listof MappingRow)
  (~>
   mappings
   (filter (λ ([r : MappingRow])
             (and (equal? (mapping-row-id r) id)
                  (equal? (mapping-row-cycle r) cc)))
           _)))

;; given an id, choose a mapping
(: id->mapping (Course-Id CatalogCycle [#:pref-order (Listof Subject)]
                          -> (List Subject CourseNum)))
(define (id->mapping id cc #:pref-order [pref-order default-subject-preference-order])
  (ensure-canonical id)
  (define rows
    (~>
     (id-mappings/cc id cc)
     ((inst sort MappingRow Subject)
      (better-subject pref-order) #:key mapping-row-subject)))
  (match rows
    [(list) (error 'id->mapping "no mapping for course ~v in cycle ~v" id cc)]
    [other (define best (car rows))
           (list (mapping-row-subject best)
                 (mapping-row-course-num best))]))

(define (id-has-subj-mapping? [id : Course-Id] [cc : CatalogCycle]
                              [subject : Subject])
  (~>
   (id-mappings/cc id cc)
   (filter (λ ([r : MappingRow])
             (equal? (mapping-row-subject r) subject))
           _)
   (null?)
   (not)))


(define default-subject-preference-order (ann '("CSC" "CPE" "DATA" "EE")
                                              (Listof Subject)))

;; given two subjects, return #t is the first is preferred to the second
(: better-subject ((Listof Subject) -> (Subject Subject -> Boolean)))
(define ((better-subject pref-order) s1 s2)
  (< (index-of/+ s1 pref-order) (index-of/+ s2 pref-order)))

(: index-of/+ (All (T) (T (Listof T) -> Natural)))
(define (index-of/+ elt l)
  (cond [(null? l) 0]
        [else (cond [(equal? elt (car l)) 0]
                    [else (add1 (index-of/+ elt (cdr l)))])]))

;; defines a mapping from course ids to strings for the purposes
;; of sorting. Interleaves CSC and CPE, and other majors
;; come later
(define (course-key [course : String]) : String
  (match course
    [(regexp #px"^(csc|cpe)([0-9]+)" (list _ _ num))
     (string-append (cast num String) "-" course)]
    [other
     (string-append "UNK-" course)]))

(module+ test
  (require typed/rackunit)

  
(check-equal? (id->mapping "csc348" "2022-2026") (list "CSC" "248"))
  (check-equal? (id->mapping "csc202" "2022-2026") (list "CSC" "202"))
  (check-equal? (id->mapping "csc202" (ann "2022-2026"
                                           CatalogCycle)
                             #:pref-order (ann '("CPE" "CSC")
                                               (Listof Subject)))
                (list "CPE" "202"))
  

  (check-equal? (canonicalize/num "2015-2017" 430) "csc430")
  (check-equal? (canonicalize/num "2015-2017" "430") "csc430")
  (check-exn #px"no match" (λ () (canonicalize/num "2015-2017" "433")))
  (check-exn #px"different matches" (λ () (canonicalize/num "2015-2017" "400")))
  (check-exn #px"different matches" (λ () (canonicalize/num "2022-2026" "441")))
  
  (check-equal? (canonicalize "2015-2017" "CPE" "430") "csc430")

  (check-equal? (canonicalize "2015-2017" 'CPE "0430") "csc430")
  
  (check-equal? (canonicalize "2015-2017" 'CPE 430) "csc430")

  (check-equal? (canonicalize/qtr 2158 "CPE" "0430") "csc430")

  (check-equal? (canonicalize/qtr/noerr 2158 "CPE" "0930") #f)

  (check-equal? (canonicalize/qtr 2158 'CPE 430) "csc430")

  (check-equal? (canonicalize "2026-2028" 'CSC "2001") "csc2001")

  (check-true (string<? (course-key "cpe100") (course-key "csc300")))
  (check-true (string<? (course-key "csc123") (course-key "cpe450")))
  (check-true (string<? (course-key "csc590") (course-key "data301")))
  (check-equal? (course-key "csc243") "243-csc243")

  (check-true (coursenum? "1000L"))

  (check-equal? (id-mappings/cc "csc4610" "2026-2028")
                '(#("2026-2028" "CSC" "4610" "csc4610")
                  #("2026-2028" "DATA" "4610" "csc4610")))
  (check-false (id-has-subj-mapping? "csc4610" "2026-2028" "CPE"))
  (check-true (id-has-subj-mapping? "csc4610" "2026-2028" "DATA"))

  )

