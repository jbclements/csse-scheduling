
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
         last-mapped-qtr)

(require/typed "fetch-mapping.rkt"
               [course-mappings Any])

(require racket/match
         racket/set
         "types.rkt"
         "qtr-math.rkt"
         (only-in racket/list remove-duplicates filter-map))

(define-type Subject
  (U "AEPS" "AERO" "AG" "AGB" "AGC" "ANT" "ARCE" "ARCH" "ART" "ASCI" "ASTR" "BIO"
     "BMED" "BOT" "BRAE" "BUS" "CD" "CE" "CHEM" "CHIN" "CM" "COMS" "CPE" "CRP"
     "CSC" "DANC" "DATA" "DSCI" "ECON" "EDES" "EDUC" "EE" "ENGL" "ENGR" "ENVE" "ERSC"
     "ES" "ESE" "ESM" "FPE" "FR" "FSN" "GEOG" "GEOL" "GER" "GRC" "GS" "GSA" "GSB" "GSE" "HCS" "HIST" "HLTH" "HNRC"
     "HNRS" "HUM" "IME" "ISLA" "IT" "ITAL" "ITP" "JOUR" "JPNS" "KINE" "LA"
     "LAES" "LS" "MATE" "MATH" "MCRO" "ME" "MLL" "MSCI" "MSL" "MU" "NR" "PEM" "PEW"
     "PHIL" "PHYS" "PLSC" "POLS" "PSC" "PSY" "RELS" "RPTA" "SCM" "SOC" "SPAN" "SS"
     "STAT" "TH" "UNIV" "WGS" "WGQS" "WLC" "WVIT" "ZOO"))

(define-type CourseNum String) ;; such as "123" or "0123"

(define-type MappingRow (Vector CatalogCycle Subject CourseNum Course-Id))

(define-predicate subject? Subject)

(: mapping-row-id (MappingRow -> Course-Id))
(define (mapping-row-id r)
  (vector-ref r 3))

;; this can't be captured by a TR type
(: coursenum? (String -> Boolean))
(define (coursenum? s)
  ;; turns out there are two-digit course numbers.
  (regexp-match? #px"^[0PS]?[0-9]{2,3}( +X)?$" s))

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
  (canonicalize (qtr->catalog-cycle qtr) subject number))


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

;; given an id, choose a mapping
(: id->mapping (Course-Id CatalogCycle -> (List Subject CourseNum)))
(define (id->mapping id cc)
  (define mappings
    (filter (λ ([mapping : (List CatalogCycle Subject CourseNum)]) : Boolean
              (equal? (car mapping) cc))
            (id-mappings id)))
  (match mappings
    ['() (error 'id->mapping "no mapping for course ~v in cycle ~v" id cc)]
    [other
     (define best-mapping
       (for/fold : (List CatalogCycle Subject CourseNum)
         ([best : (List CatalogCycle Subject CourseNum) (car mappings)])
         ([m : (List CatalogCycle Subject CourseNum) (cdr mappings)])
         (cond [(better-mapping? m best) m]
               [else best])))
     (list (cadr best-mapping) (caddr best-mapping))]))

(define (better-mapping? [a : (List CatalogCycle Subject CourseNum)]
                         [b : (List CatalogCycle Subject CourseNum)]) : Boolean
  (cond [(equal? (cadr a) (cadr b))
         (error 'better-mapping? "comparing two mappings with same subject: ~e and ~e"
                a b)]
        [(equal? (cadr a) "CSC") #t]
        [(equal? (cadr b) "CSC") #f]
        [(equal? (cadr a) "CPE") #t]
        [(equal? (cadr b) "CPE") #f]
        [else
         (string<? (cadr a) (cadr b))]))



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

  (check-true (string<? (course-key "cpe100") (course-key "csc300")))
  (check-true (string<? (course-key "csc123") (course-key "cpe450")))
  (check-true (string<? (course-key "csc590") (course-key "data301")))
  (check-equal? (course-key "csc243") "243-csc243"))

