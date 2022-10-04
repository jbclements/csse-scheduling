#lang typed/racket

;; this file defines the coordinators for CSSE courses.
;; specifically, it maps canonical course names to a
;; list of instructor last names. Courses with no coordinator
;; are associated with empty lists.

;; checks to make sure no course appears more than once. Does not
;; check that all courses are listed (see stale-coordinators.rkt)

(require/typed csv-reading
               [csv->list (Input-Port -> (Listof (Listof Any)))])

(require "canonicalize.rkt"
         racket/runtime-path)

(define-runtime-path here ".")

(define current-catalog : CatalogCycle "2020-2021")

;; a mapping from canonical course name to the last names of the
;; instructors that are the coordinators for that course
(provide coordinators)

(define-type CoordinatorRecord (List String (Listof String)))
(define-type CoordinatorList (Listof CoordinatorRecord))

(define-predicate lostrstrstr? (Listof (List String String String)))

(define raw-coordinators
  (call-with-input-file (build-path here "coordinators.csv") csv->list))

(define coordinator-strs : (Listof (List String String String))
  (cond [(lostrstrstr? raw-coordinators) raw-coordinators]
        [else (error 'coordinators-file
                     "expected csv where each line has 3 strings, got: ~e"
                     raw-coordinators)]))

(define (coordinators [cc : CatalogCycle])
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
        (match l
          [(list _ _ name)
           (regexp-split #px"/" name)]
          [(list _ _ "") #f]))
      (list canonical-name coordinator)))
  ;; ensure that no course is listed more than once:
  (define possible-duplicate (check-duplicates (map (inst first String) coordinators-list)))
  (when possible-duplicate 
    (error 'duplicate "coordinators list contains duplicate: ~v\n"
           possible-duplicate))

  coordinators-list)




