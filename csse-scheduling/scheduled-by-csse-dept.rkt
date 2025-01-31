#lang typed/racket/base


(provide
 ;; this is out-of-date. Try to replace it with non-supervisory-computing-courses
 ;courses-we-schedule ; used?
 non-supervisory-computing-courses
 ;; includes any courses cross-listed as csc:
 non-supervisory-csc-courses
 non-supervisory-cpe-courses
 non-supervisory-ee-courses
 supervisory-courses ; used? yep.
 cycle-course-configuration
 cycle-course-wtus
 cycle-course-wtus/noerror
 2021-course-set
 2022-course-set
 )

;; cycles before this can be ignored for the purposes of determining
;; a subject for a number...
(define earliest-cycle-cutoff : CatalogCycle "2015-2017")
;; used to determine the list of courses to be scheduled
(define current-catalog-cycle "2022-2026")

(define computing-subjects '("CSC" "CPE" "SE" "EE" "DATA"))

(define-type Configuration String)

(require "types.rkt"
         "canonicalize.rkt"
         "qtr-math.rkt"
         "credentials.rkt"
         "ownership.rkt"
         racket/set
         racket/list
         racket/match)



(require/typed "fetch-mapping.rkt"
               [cycle-course-configurations
                (Listof (Pair CatalogCycle
                              (Listof (Pair Course-Id Configuration))))]
               [course-mappings
                (Listof Course-Mapping)])
#;(require/typed "ownership.rkt"
               [2022-cpe-courses
                (Listof Course-Id)])
(define 2022-cpe-courses : (Listof Course-Id) '("bogus"))

(define-type Course-Mapping (Vector CatalogCycle String String Course-Id))
(define (mapping-cycle [cm : Course-Mapping]) : CatalogCycle
  (vector-ref cm 0))
(define (mapping-subject [cm : Course-Mapping]) : String
  (vector-ref cm 1))
(define (mapping-num [cm : Course-Mapping]) : String
  (vector-ref cm 2))
(define (mapping-id [cm : Course-Mapping]) : Course-Id
  (vector-ref cm 3))


;; should this be parameterized by the catalog cycle? sigh...


;; wait... can't this be determined by informatino from fetch-mapping? Urg, maybe not.
;; these are supervisory courses, so they aren't scheduled
;; by the scheduler
;; just checked this using the code in one-off-scripts/supervisory-courses
(define supervisory-courses : (Setof String)
  (list->set
   (map
    ensure-canonical
    '("cpe200"
      "cpe400"
      "cpe461"
      "cpe462"
      "cpe493"
      "cpe494"
      "cpe495"
      "csc200"
      "csc400"
      "csc491"
      "csc492"
      "csc497"
      "csc498"
      "csc493"
      "csc494"
      "csc495"
      "csc500"
      "csc593"
      "csc594"
      "csc595"
      "csc596"
      "csc597"
      "csc599"
      "ee200"
      "ee400"
      "ee461"
      "ee462"
      "ee493"
      "ee494"
      "ee495"
      "ee500"
      "ee594"
      "ee595"
      "ee599"))))

;; return non-supervisory courses that have one of these subjects
;; (these lists will overlap for cross-listed courses)
(define (non-sup-courses-in-subjects [subjects : (Listof String)])
  : (Setof Course-Id)
  (set-subtract
   (list->set
    (map
     mapping-id
     (filter
      (λ ([cm : Course-Mapping])
        (and (equal? (mapping-cycle cm) current-catalog-cycle)
             (member (mapping-subject cm) subjects)))
      course-mappings)))
   supervisory-courses))

(define non-supervisory-computing-courses
  (non-sup-courses-in-subjects computing-subjects))

(define known-non-schedulable-courses (set "cpe345" "cpe488"))
;; checking up on this mapping
;; these are courses in our catalogs with one of the given subjects
;; that are not supervisory but do not appear on any of the CLSS pages;
;; in other words, they cannot be scheduled by any of the four departments.;
;; It turns out there *are* some of theses
(unless
    (subset?
     (set-subtract non-supervisory-computing-courses
                   (list->set
                    (set-union cs-dept-courses
                               cpe-dept-courses
                               ee-dept-courses
                               data-dept-courses)))
     known-non-schedulable-courses)
  (error 'ouch "there are apparently other courses that can't be scheduled 12442312"))

;; are there courses that can be scheduled that don't appear in our catalog?
(unless (set-empty?
         (set-subtract (set-subtract
                        (list->set
                         (set-union cs-dept-courses
                                    cpe-dept-courses
                                    ee-dept-courses
                                    data-dept-courses))
                        supervisory-courses)
                       non-supervisory-computing-courses))
  (error 'ouch "moreinfo3412341"))


(define non-supervisory-csc-courses
  (set-subtract (list->set cs-dept-courses) supervisory-courses))
(define non-supervisory-cpe-courses
  (set-subtract (list->set cpe-dept-courses) supervisory-courses))
;; Lynne is confident that the overlaps belong to CPE:
(define non-supervisory-ee-courses
  (set-subtract
   (set-subtract (list->set ee-dept-courses) supervisory-courses)
   (list->set cpe-dept-courses)))


;; for 2021-2022 planning, I'm going to add together
;; the csc courses and the CPE courses that aren't cross
;; listed with ee. Yikes.
(define cpe-extra
  (set-subtract non-supervisory-cpe-courses
                (set-union non-supervisory-csc-courses
                           non-supervisory-ee-courses)))
;; this is pretty temporary...
(define 2021-course-set (set-union non-supervisory-csc-courses cpe-extra))

;; for 2022-2026 planning, we have an explicit list of CPE courses from Lynne
(define 2022-course-set (set-union non-supervisory-csc-courses
                                   non-supervisory-cpe-courses))

;; map numbers to ids to allow short-cuts in schedule description.
;; for instance, we can just write "430" rather than "csc430".
(define num-id-table
  (let ()
    
    (define (newer-than-cutoff [cc : CatalogCycle])  : Boolean
      (not (catalog-cycle-<? cc earliest-cycle-cutoff)))
    (define csc-cpe-mappings
      (filter (λ ([cm : Course-Mapping])
                (and
                 (member (mapping-subject cm) '("CSC" "CPE"))
                 (newer-than-cutoff (mapping-cycle cm))))
              course-mappings))
    (make-immutable-hash
     (map (λ ([ms : (Listof Course-Mapping)])
            (ann (cons (mapping-num (first ms))
                       (remove-duplicates
                        (map mapping-id ms)))
                 (Pairof String (Listof Course-Id))))
          (group-by mapping-num csc-cpe-mappings)))))

(module+ test
  (require typed/rackunit)
  (check-equal? (hash-ref num-id-table "100") '("cpe100"))
  (check-equal? (hash-ref num-id-table "350") '("csc350" "cpe350")))

;; given a course and a catalog cycle, return its configuration string
(define (cycle-course-configuration [course : Course-Id] [cycle : CatalogCycle]) : (U False Configuration)
  (define configurations
    (match (assoc cycle cycle-course-configurations)
      [(cons _ c) c]
      [#f (error 'cycle-course-configuration
                 "no configuration info for cycle ~v" cycle)]))
  (match (assoc course configurations)
    [#f #f]
    [(cons id configuration) configuration]))

(define-predicate false? False)

;; given a course, return the number of WTUs required to teach it
;; in the 2019-2021 catalog, or return #f if it's nonstandard
(define (cycle-course-wtus/noerror [cycle : CatalogCycle] [course : Course-Id] [lab-mult : Natural 1])
  : (U False Nonnegative-Exact-Rational)
  (define maybe-configuration (cycle-course-configuration course cycle))
  (cond
    [(not maybe-configuration) (error 'cycle-course-wtus "no mapping found for course: ~e" course)]
    [else
     (define maybe-wtus (configuration->wtus maybe-configuration lab-mult))
     (cond [maybe-wtus maybe-wtus]
           [else #f])]))

;; same as above, but signal an error if nonstandard
(define (cycle-course-wtus [cycle : CatalogCycle] [course : Course-Id] [lab-mult : Natural 1])  : Nonnegative-Exact-Rational
   (or (cycle-course-wtus/noerror cycle course lab-mult)
       (error 'cycle-course-wtus
              "nonstandard configuration for course: ~e"
              course)))

(define lecture-unit-wtus : Positive-Exact-Rational #e1.0)
(define lab-unit-wtus : Positive-Exact-Rational #e2.0)
(define activity-unit-wtus : Positive-Exact-Rational #e1.3)

;; return the number of wtus associated with a course. For mega-sections, we multiply
;; the lab WTUs, on the assumption that mega-sections still have normal-sized labs
(define (configuration->wtus [c : Configuration] [lab-mult : Natural]) : (U False Nonnegative-Exact-Rational)
  (match c
    [(regexp #px"^([0-9]+)-([0-9]+)-([0-9]+)$"
             (list _ lectures labs activities))
     ;; casts should all succeed by regexps in pattern
     (+ (* lecture-unit-wtus      (cast (string->number (cast lectures String))   Nonnegative-Exact-Rational))
        (* lab-unit-wtus lab-mult (cast (string->number (cast labs String))       Nonnegative-Exact-Rational))
        (* activity-unit-wtus     (cast (string->number (cast activities String)) Nonnegative-Exact-Rational)))]
    [other #f]))


(module+ test

  
  (check-equal? (canonicalize/num "2022-2026" 123) "csc123")
  (check-exn #px"different matches for number"
             (λ () (canonicalize/num "2022-2026" 290)))
  (check-equal? (canonicalize/num "2022-2026" 431) "csc431")
  (check-equal? (canonicalize/num "2022-2026" 315) "cpe315")

  (check-equal? (cycle-course-wtus (ann "2019-2020" CatalogCycle) "csc101") 5)
  (check-equal? (cycle-course-wtus (ann "2019-2020" CatalogCycle) "csc232") #e3.3))


