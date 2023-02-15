#lang typed/racket

;; this file defines the coordinators for CSSE courses.
;; specifically, it maps canonical course names to a
;; list of instructor last names. Courses with no coordinator
;; are associated with empty lists.

;; the file should be a CSV file, with three columns.
;; the first column is a course name, written either as
;; e.g. "CSC 442" or "CSC/CPE 451"
;; the second column is the name of the course. This might not be checked....
;; the third column is either the string representation of a value in
;; (Listof (U Symbol String (List Symbol)))
;; e.g. "(Clements (csse-dept-chair))"
;; or a string that doesn't start with a paren, in which case it's
;; just a last name e.g. "Stanchev"

;; checks to make sure no course appears more than once. Does not
;; check that all courses are listed (see stale-coordinators.rkt)

(require/typed csv-reading
               [csv->list (Input-Port -> (Listof (Listof Any)))])

(require "canonicalize.rkt"
         racket/runtime-path
         typed/rackunit)

(define-runtime-path here ".")

(define current-catalog : CatalogCycle "2020-2021")

;; a mapping from canonical course name to the last names of the
;; instructors that are the coordinators for that course
(provide coordinators)

;; contains a canonical course name and a list of names & designators (either last names or special
;; identifiers like 'cpe-dept-chair
(define-type CoordinatorRecord (List String (Listof (U String Symbol))))
(define-type SpecialCoordinator (U 'csse-dept-chair
                                   'csse-grad-coordinator
                                   'cpe-dept-chair
                                   'ee-dept-chair))

(define-type CoordinatorList (Listof CoordinatorRecord))

(define-predicate lostrstrstr? (Listof (List String String String)))
(define-predicate special-coordinator? SpecialCoordinator)

(define raw-coordinators
  (call-with-input-file (build-path here "coordinators.csv") csv->list))

(define coordinator-strs : (Listof (List String String String))
  (cond [(lostrstrstr? raw-coordinators) raw-coordinators]
        [else (error 'coordinators-file
                     "expected csv where each line has 3 strings, got: ~e"
                     raw-coordinators)]))

;; read a single value from a string, ensure that it's empty afterawrd
(define (string->value [s : String])
  (define ip (open-input-string s))
  (define r (read ip))
  (define leftover (port->string ip))
  (unless (equal? (string-trim leftover) "")
    (error 'string->value "leftover non-blank text: ~e" leftover))
  r)


(define (parse-coordinators [str : String]) : (Listof (U String Symbol))
  (match str
    [(and s (regexp #px"^\\("))
     (define v (string->value s))
     (cond [(list? v)
            (for/list ([e v])
              (match e
                [(? symbol? e) (symbol->string e)]
                [(? string? s) s]
                [(list (? special-coordinator? sc)) sc]
                [other (error 'parse-coordinators "expected legal coordinator, got ~e"
                              e)]))]
           [else (error 'parse-coordinators "internal error 117729837018")])]
    [else (list str)]))

(check-equal? (parse-coordinators "Knight") '("Knight"))
(check-equal? (parse-coordinators "(Knight Queen)") '("Knight" "Queen"))
(check-equal? (parse-coordinators "(Knight (csse-dept-chair) Queen)") '("Knight" csse-dept-chair "Queen"))
(check-equal? (parse-coordinators "(\"biggy biggy biggy\")") '("biggy biggy biggy"))

(define (coordinators [cc : CatalogCycle]) : CoordinatorList
  (define coordinators-list
    (for/list : CoordinatorList ([l (in-list coordinator-strs)])
      (define canonical-name
        (match (string-trim (first l))
          ;; 2 ad-hoc fixups:
          ;["CSC 419" (canonicalize current-catalog "CPE" "419")]
          [(regexp #px"^([A-Z]+)(/[A-Z]+)? ([0-9]{3})$" (list _ subj subj2 num))
           (cond [(subject? subj)
                  (define canonicalized
                    (canonicalize cc subj (assert num string?)))
                  (when subj2
                    (define canonicalized2
                      (canonicalize cc (substring subj2 1)
                                    (assert num string?)))
                    (unless (equal? canonicalized canonicalized2)
                      (error 'canonicalize
                             "different subjects lead to different canonical \
courses: ~e"
                             (list canonicalized canonicalized2))))
                  canonicalized]
                 [else (error 'coordinators
                              "expected subject, got: ~e"
                              subj)]
                 )]))
      (define coordinator
        (parse-coordinators (third l)))
      (list canonical-name coordinator)))
  ;; ensure that no course is listed more than once:
  (define possible-duplicate (check-duplicates (map (inst first String) coordinators-list)))
  (when possible-duplicate 
    (error 'duplicate "coordinators list contains duplicate: ~v\n"
           possible-duplicate))

  coordinators-list)




