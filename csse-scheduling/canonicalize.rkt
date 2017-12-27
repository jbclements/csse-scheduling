#lang typed/racket/base

;; this file maps subject/number pairs to canonical ids.
;; at this point, it's sort of turning into a thin wrapper
;; for the scheduling database.

(provide canonicalize
         canonicalize/qtr
         canonical-id?
         ensure-canonical
         courses-in-subject
         course-key
         id-mappings
         Subject
         CourseNum
         CourseID
         subject?
         mappings
         MappingRow
         CatalogCycle)

(require/typed "fetch-mapping.rkt"
               [course-mappings Any])

(require racket/match
         racket/set
         "qtr-math.rkt"
         (only-in racket/list remove-duplicates))

(define-type Subject (U "CPE" "CSC" "HNRS" "ART" "EE" "IME" "MATE"
                        "DATA"))
(define-type CourseNum String) ;; such as "123" or "0123"


(define-type CourseID String) ;; such as "csc123"

(define-type MappingRow (Vector CatalogCycle Subject CourseNum CourseID))

(define-predicate subject? Subject)

(: mapping-row-id (MappingRow -> CourseID))
(define (mapping-row-id r)
  (vector-ref r 3))

;; this can't be captured by a TR type
(: coursenum? (String -> Boolean))
(define (coursenum? s)
  (regexp-match? #px"^0?[0-9]{3}( +X)?$" s))

;; the rows in the table, representing mappings from offering
;; to canonical name
(define mappings : (Listof MappingRow)
  (cast course-mappings (Listof MappingRow)))


;; a hash table to speed up the mapping, and also to check that the
;; rows that we got from the db conform to the contracts that we
;; specifiye
(define mapping-hash
  (for/hash : (Immutable-HashTable (Vector CatalogCycle Subject CourseNum)
                                   CourseID)
    ([v (in-list mappings)])
    (match-define (vector cycle subject coursenum name) v)
    (unless (coursenum? coursenum)
      (error "unexpected values in db row ~e" v))
    (values (vector cycle subject coursenum) name)))



;; given catalog cycle, subject, and number, return a string
;; representing the canonical course name
(: canonicalize (CatalogCycle (U Symbol String) (U Natural CourseNum) -> CourseID))
(define (canonicalize cycle subject-input number-input)
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
      [(regexp #px"^0[0-9]{3}" (list _))
       (substring number 1)]
      [other number]))
  (hash-ref mapping-hash (vector cycle subject trimmed-number)
            (λ ()
              (error 'canonicalize
                     "no mapping found for offering: ~e"
                     (list cycle subject number)))))

;; like canonicalize, but accepts qtr rather than catalog cycle
(: canonicalize/qtr (Natural (U Symbol String) (U Natural String) -> CourseID))
(define (canonicalize/qtr qtr subject number)
  (canonicalize (qtr->cycle qtr) subject number))


;; is this string the canonical id of some course?
(define (canonical-id? [n : CourseID]) : Boolean
  (set-member? canonical-ids n))

(define canonical-ids : (Setof CourseID)
  (list->set (hash-values mapping-hash)))

(define (row-name [r : MappingRow]) : String
  (vector-ref r 3))

(define (row-num [r : MappingRow]) : String
  (vector-ref r 2))

;; ensure that a course id is in the canonical list
;; (this defn must be early to allow its use in courses-we-schedule)
(define (ensure-canonical [s : String]) : String
  (cond [(canonical-id? s) s]
        [else (raise-argument-error
               'ensure-canonical
               "canonical course id"
               0 s)]))



;; return the canonical names of all classes in the given catalog offered
;; with the given subject
(: courses-in-subject (CatalogCycle Subject -> (Listof CourseID)))
(define (courses-in-subject cycle subject)
  (remove-duplicates
   (map row-name
        (filter (λ ([r : MappingRow])
                  (and (equal? (vector-ref r 0) cycle)
                       (equal? (vector-ref r 1) subject)))
                mappings))))

;; return all of the mappings for a given class id
(: id-mappings (CourseID -> (Listof (List CatalogCycle Subject CourseNum))))
(define (id-mappings id)
  (define rows
    (filter (λ ([r : MappingRow])
              (equal? (mapping-row-id r) id))
            mappings))
  (map (λ ([r : MappingRow])
         (list (vector-ref r 0)
               (vector-ref r 1)
               (vector-ref r 2)))
       rows))


;; defines a mapping from course ids to strings for the purposes
;; of sorting. Interleaves CSC and CPE, and other majors
;; come later
(define (course-key [course : String]) : String
  (match course
    [(regexp #px"^[a-zA-Z]+([0-9]+)" (list _ num))
     (string-append (cast num String) "-" course)]
    [other
     (string-append "UNK-" course)]))

(module+ test
  (require typed/rackunit)

  (check-equal? (canonicalize "2015-2017" "CPE" "430") "csc430")

  (check-equal? (canonicalize "2015-2017" 'CPE "0430") "csc430")
  
  (check-equal? (canonicalize "2015-2017" 'CPE 430) "csc430")

  (check-equal? (canonicalize/qtr 2158 "CPE" "0430") "csc430")

  (check-equal? (canonicalize/qtr 2158 'CPE 430) "csc430")

  (check-equal? (course-key "csc243") "243-csc243"))

