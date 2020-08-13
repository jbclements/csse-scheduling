#lang typed/racket/base

;; this file contains information related to student requirements
;; for graduation

(require "../canonicalize.rkt"
         "../course-listings.rkt"
         "../types.rkt"
         racket/list
         racket/match
         racket/format)

(provide Requirement
         ReqFun
         program-requirements
         pass-requirement
         pass-with-c-requirement
         pass-in-qtr-requirement
         pass-with-c-in-qtr-requirement
         took-in-qtr-requirement
         missing-requirements

         passed-data-structures?
         passed-bigger-projects?

         passing-grade?
         c-passing-grade?)

;; given a list of grades, a course id, and a predicate mapping a grade to
;; a boolean, return a list of lists of grade-records indicating the
;; ways that the requirement could be satisfied.
(define (passed-pred [course-id : Course-Id]
                     [pred : (Grade -> Boolean)]) : ReqFun
  (check-course course-id)
  (define passed-course?
    (λ ([gr : Grade-Record])
      (and (equal? (second gr)
                   course-id)
           (pred (gr-grade gr)))))
  (satisfied-pred passed-course?))

;; given a list of grades, and a predicate indicating whether a
;; Grade-Record satisfies the requirement, return a list of
;; lists of grade-recrods indicating the ways that the requirement
;; could be satisfied
(define (satisfied-pred [pred : (Grade-Record -> Boolean)]) : ReqFun
  (λ ([student-grades : (Listof Grade-Record)])
    (define satisfying-rows
      (filter pred student-grades))
    (for/list ([row (in-list satisfying-rows)])
      (remove row student-grades))))

;; is this a canonical course id?
(define (check-course [course-id : String]) : Void
  (unless (canonical-id? course-id)
    (raise-argument-error 'check-course
                          "canonical course id"
                          0 course-id))
  (void))

(define c-passing-grades : (Listof String)
  '("A" "A-" "B+" "B" "B-" "C+" "C" "C-"))

(define passing-grades : (Listof String)
  '("A" "A-" "B+" "B" "B-" "C+" "C" "C-" "D+" "D" "D-" "CR"))

(define (grade-names->pred [grade-names : (Listof Grade)]) :
  (Grade -> Boolean)
  (λ ([g : Grade]) (not (not (member g grade-names)))))

(define passing-grade?
  (grade-names->pred passing-grades))

(define c-passing-grade?
  (grade-names->pred c-passing-grades))

(define (any-grade? [g : Grade]) : Boolean
  #t)

;; Requirements are interesting; for most requirements, students
;; cannot fulfill multiple requirements using the same course.
;; accordingly, determining whether a collection of requirements
;; is met requires "assigning" grades to certain requirements. To
;; model this, requirements accept a list of grades and return
;; a list of possible ways in which the requirement is satisfied,
;; where a "way" is represented as a list of the course grades not
;; yet "used" by the requirement.

;; a ReqFun represents a requirement test; it accepts a list of
;; grades, and returns a list of lists of grade-records,
;; indicating the ways in which that requirement could "use up"
;; the student's grades. The empty list therefore indicates failure.

;; Unsurprisingly, this leads to some pretty catastrophic exponential
;; blow-up. Accordingly, we use the or!/req function which cuts off
;; exploration at the first success. I believe this is a reasonable
;; analog of prolog's "cut" operator.

(define-type ReqFun ((Listof Grade-Record) -> (Listof (Listof Grade-Record))))

(define-type Requirement (List ReqName ReqFun))

;; time for some combinators:

;; given a course id,
;; did the student pass the given course with a grade of C- or better?
(define (passc/req [course-id : Course-Id]) : ReqFun
  (passed-pred course-id c-passing-grade?))

;; given a course id,
;; did the student pass the given course at all?
(define (pass/req [course-id : Course-Id]) : ReqFun
  (passed-pred course-id passing-grade?))

;; given a course id, did the student take the class
;; (not necessarily finished, but definitely qualified for)
(define (took/req [course-id : Course-Id]) : ReqFun
  (passed-pred course-id any-grade?))

(define (pass-with-grade-in-qtr/req [course-id : Course-Id] [qtr : Qtr]
                                    [grade-pred : (Grade -> Boolean)]) : ReqFun
  (check-course course-id)
  (satisfied-pred (λ ([gr : Grade-Record])
                    (and (= (gr-qtr gr) qtr)
                         (equal? (gr-course gr) course-id)
                         (grade-pred (gr-grade gr))))))

(define (pass-in-qtr/req [course-id : Course-Id] [qtr : Qtr]) : ReqFun
  (pass-with-grade-in-qtr/req course-id qtr passing-grade?))

(define (pass/c-in-qtr/req [course-id : Course-Id] [qtr : Qtr]) : ReqFun
  (pass-with-grade-in-qtr/req course-id qtr c-passing-grade?))

;; given a course id, did the student take the course?
(define (took-in-qtr/req [course-id : Course-Id] [qtr : Qtr]) : ReqFun
  (pass-with-grade-in-qtr/req course-id qtr (λ (x) #t)))

;; combine two requirement-funs with an 'or'
(define (or/req [rf1 : ReqFun] [rf2 : ReqFun])
  (λ ([g : (Listof Grade-Record)])
    (remove-duplicates (append (rf1 g) (rf2 g)))))

;; short-cutting or; used to control exponential explosion.
;; use only when the first possible hit is always okay.
(define (or!/req [rf1 : ReqFun] [rf2 : ReqFun])
  (λ ([g : (Listof Grade-Record)])
    (define ways1 (rf1 g))
    (cond [(empty? ways1)
           (rf2 g)]
          [else ways1])))

;; combine two requirement-funs with an 'and, assuming
;; the requirements must be satisfied by different course sets
(define (and/req [rf1 : ReqFun] [rf2 : ReqFun]) : ReqFun
  (λ ([g : (Listof Grade-Record)])
    (define ways1 (rf1 g))
    (remove-duplicates
     (apply
      append
      (for/list : (Listof (Listof (Listof Grade-Record)))
        ([way (in-list ways1)])
        (rf2 way))))))


;; given a list of courses, return the first success.
;; eliminates lots of garbage collection
(define (or!/courses/req [courses : (Listof Course-Id)]) : ReqFun
  (λ ([g : (Listof Grade-Record)])
    (or (for/or : (U False (Listof (Listof Grade-Record)))
          ([course (in-list courses)])
          (define try ((pass/req course) g))
          (cond [(empty? try) #f]
                [else try]))
        '())))

;; make a "ghost" requirement that checks the requirement but
;; does not deduct the used courses from the list of grades available.
;; useful as e.g. when passing 102 is used as evidence that student
;; has 101 credit
(define (ghost/req [rf : ReqFun]) : ReqFun
  (λ ([g : (Listof Grade-Record)])
    (cond [(not (empty? (rf g))) (list g)]
          [else '()])))

;; has the student passed the "bigger projects"/OO course?
(define passed-bigger-projects? : ReqFun
  (or!/req
   (or!/req (passc/req "csc102")
            (passc/req "csc203"))
   (ghost/req (took/req "csc357"))))

;; has the student passed the data structures course?
(define passed-data-structures? : ReqFun
  (or!/req
   (or!/req (passc/req "csc202")
            (passc/req "csc103"))
   (ghost/req (took/req "csc357"))))

;; passed-101 : has the student passed 101 or taken a later course
;; indicating that they didn't need it?
(define passed-101?
  (or!/req (passc/req "csc101")
           (ghost/req
            (or!/req
             (or!/req passed-bigger-projects?
                      passed-data-structures?)
             ;; add a "just took it" requirement
             (took/req "csc202")))))

(define passed-123? (pass/req "csc123"))

;; did this csc student pass the SE requirement? (307 OR 308+309)
(define passed-csc-se-req? : ReqFun
  (or!/req (pass/req "csc307")
          (and/req (pass/req "csc308")
                   (pass/req "csc309"))))

;; did this csc student pass the 'discrete' requirement (141 or 348)?
(define passed-discrete? : ReqFun
  (or!/req (pass/req "csc141")
           (or!/req (pass/req "csc348")
                    (ghost/req (took/req "csc349")))))

(define passed-one-of or!/courses/req)

;; did this student get 4 units total from csc400, cpe400, or csc490/496.
(define got-special-problems-credit? : ReqFun
  (λ ([g : (Listof Grade-Record)])
    (define special-problems-courses
      '("csc400" "cpe400" "csc490" "csc496"))
    (for-each check-course special-problems-courses)
    (define-values (special-topics-grades other-grades)
      (partition (λ ([g : Grade-Record]) (member (gr-course g)
                                                 special-problems-courses))
                 g))
    (define special-topics-units (apply + (map gr-units special-topics-grades)))
    (cond [(<= 4 special-topics-units)
           ;; yay, they get credit for them, take them out of the pool
           (list other-grades)]
          ;; no, fail:
          [else '()])))

;; got 4 units of 400?
(define got-4-units-of-400? : ReqFun
  (λ ([g : (Listof Grade-Record)])
    (define special-problems-courses '("cpe400"))
    (for-each check-course special-problems-courses)
    (define-values (special-topics-grades other-grades)
      (partition (λ ([g : Grade-Record])
                   (member (gr-course g)
                           special-problems-courses))
                 g))
    (define special-topics-units
      (apply + (map gr-units special-topics-grades)))
    (cond [(<= 4 special-topics-units)
           ;; yay, they get credit for them, take them out of the pool
           (list other-grades)]
          ;; no, fail:
          [else '()])))


;; how many units did this student earn from passing the course
(define (total-passed-units [g : (Listof Grade-Record)] [course : Course-Id])
  (check-course course)
  (apply + (map gr-units
                (filter (λ ([g : Grade-Record]) (equal? (gr-course g) course))
                        g))))

;; represents the requirement for a technical elective
(define (passed-technical-elective? [cc : CatalogCycle]) : ReqFun
  (or!/courses/req (hash-ref csc-te-course-table cc)))

;; represents the requirement for an upper-level technical elective
(define (passed-upper-level-technical-elective? [cc : CatalogCycle]) : ReqFun
  (or!/courses/req (hash-ref csc-ul-te-course-table cc)))

;; represents the requirement for a technical elective
(define (passed-se-technical-elective? [cc : CatalogCycle]) : ReqFun
  (or!/courses/req (hash-ref se-te-course-table cc)))

;; represents the requirement for an upper-level technical elective
(define (passed-upper-level-se-technical-elective? [cc : CatalogCycle]) : ReqFun
  (or!/courses/req (hash-ref se-ul-te-course-table cc)))

;; passed one of the lec-lab courses or a lec and a lab separately.
;; this abstraction is just barely okay.
(define (passed-ee-te-lec-lab-req? [cc : CatalogCycle]) : ReqFun
  (or!/req
   (or!/courses/req (hash-ref ee-te-lec-lab-table cc))
   (and/req
    (or!/courses/req (hash-ref ee-te-lec-table cc))
    (or!/courses/req (hash-ref ee-te-lab-table cc)))))

;; passed the "final" ee TE requirement; there's inaccuracy
;; in e.g. disallowing taking 3 labs, assuming that the "other"
;; courses are all at least 3 units, etc.
(define (passed-ee-te-open-req? [cc : CatalogCycle]) : ReqFun
  (or!/req
   (or!/courses/req (hash-ref ee-te-lec-lab-table cc))
   (or!/req
    (or!/courses/req (hash-ref ee-te-lec-table cc))
    (or!/courses/req (hash-ref ee-te-other-table cc)))))

;; is this just longer because I hadn't built the abstraction yet?
;; short-cutting: find a cpe TE and remove it from the list
(define (passed-cpe-technical-elective? [cc : CatalogCycle]) : ReqFun
  (let ()
    (define cpe-te-courses (hash-ref cpe-te-course-table cc))
    (λ ([grs : (Listof Grade-Record)])
      (define success-grade : (U False Grade-Record)
        (let loop ([grs grs])
          (cond [(empty? grs) #f]
                [else
                 (define g (first grs))
                 (cond
                   [(and (member (gr-course g) cpe-te-courses)
                         (passing-grade? (gr-grade g)))
                    g]
                   [else (loop (rest grs))])])))
      (cond [success-grade (list (remove success-grade grs))]
            ['()]))))


;; signal an error if two requirements have the same name
(define (ensure-distinct-names [reqs : (Listof Requirement)]) : (Listof Requirement)
  (match (check-duplicates (map (inst first ReqName) reqs))
    [#f reqs]
    [other (error 'ensure-distinct
                  "duplicate requirement name: ~e"
                  other)]))

;; CAVEAT: NO WAY TO KNOW IF THE STUDENTS WILL TAKE AN EXTERNAL TE

(define-syntax ccparam
  (syntax-rules ()
      [(_ id exp) (λ ([id : CatalogCycle]) exp)]))

;; making a big table prevents bad collisions
(define req-table : (Immutable-HashTable Symbol (CatalogCycle -> ReqFun))
  (make-immutable-hash
   `((cpe-TE/400 . ,(ccparam
                     cc
                     (or!/req got-4-units-of-400?
                              (passed-cpe-technical-elective? cc))))
     (cpe-TE-123 . ,(ccparam cc
                      (or!/req passed-123?
                               (passed-cpe-technical-elective? cc))))
     (cpe-arch . ,(ccparam _ (passed-one-of '("cpe315" "cpe333"))))
     (microcon . ,(ccparam _ (passed-one-of '("cpe329" "cpe316" "cpe336"))))
     (cpe-sp-1 . ,(ccparam _ (passed-one-of '("cpe461" "csc497"))))
     (cpe-sp-2 . ,(ccparam _ (passed-one-of '("cpe462" "csc498"))))
     (ethics .   ,(ccparam _ (passed-one-of '("csc300" "phil323"))))
     (csc-sp-1 . ,(ccparam _ (passed-one-of '("csc491" "csc497"))))
     (csc-sp-2 . ,(ccparam _ (passed-one-of '("csc492" "csc498")))))))

(define (req-lookup [spec : (U Symbol String)] [cc : CatalogCycle]) : ReqFun
  ((hash-ref req-table spec) cc))

(define ((spec->rec [cc : CatalogCycle]) [spec : (U String Symbol)]) : Requirement
  (cond [(string? spec) (req spec)]
        [(symbol? spec) (list (list spec) (req-lookup spec cc))]))

(define (all-of-these [cc : CatalogCycle]
                      [s : (Listof (U String Symbol))])
  (map (spec->rec cc) s))

;; make a list of technical elective requirements for a given major
(define (make-TE-requirements [prefix : String] [req : ReqFun] [n : Natural])
  (for/list : (Listof Requirement)
          ;; one more for the loss of 431
          ([i (in-range n)])
          (list (list (string->symbol (~a prefix"-TE-" i)))
                req)))

;; FIXME 
(define (make-csc-te-reqs [cc : CatalogCycle] [n : Natural]) : (Listof Requirement)
        (make-TE-requirements "csc" (passed-technical-elective? cc) n))


(define (req [course-id : Course-Id]) : Requirement
  (list course-id (pass/req course-id)))

(define computing-common-requirements
  : (Listof Requirement)
  (list (list "csc101" passed-101?)
        (list "csc202" passed-data-structures?)
        (list "csc203" passed-bigger-projects?)
        (req "csc357")))

;; NB because of the "cut" style of checking, it's important to put
;; technical electives last; otherwise, a course like 307 could be
;; counted as a technical elective rather than satisfying the SE requirement.

(define (common-csc-requirements [cc : CatalogCycle])
  (append
   computing-common-requirements
   ;; NB: 123 is treated like a TE to allow transfer students not to take it.
   (list (req  "csc225")
         ;; this is an approximation... apparently, students taking 308
         ;; are *required* to take 308. We can express this using our
         ;; system, but interpreting the results could be hard, because
         ;; students that are lacking the csc-SE requirement might be
         ;; lacking both, or just 309. It would look somethnig like this...
         #;(list '(csc-SE) (or!/req (and!/req (pass/req "csc307" passed-technical-elective?))
                                    (and!/req (pass/req "csc308" "csc309"))))
         ;; this is the simplified version:
         (list '(csc-SE) (passed-one-of '("csc307" "csc308")))
         (req  "cpe315")
         (list "csc348" passed-discrete?)
         (req  "csc349")
         (req "csc430")
         (req "csc445")
         (req "csc453")
         ;; this must occur before other TE requirements or the greedy
         ;; nature of matching could mark a satisfied requirement
         ;; as unsatisfied
         (list '(upper-level-csc-TE)
               (passed-upper-level-technical-elective? cc))
         (list '(csc-TE/123) (or!/req passed-123?
                                      (passed-technical-elective? cc)))
         (list '(csc-TE/special-problems)
               (or!/req got-special-problems-credit?
                        (passed-technical-elective? cc))))))

;; the master list of requirements
(define (common-se-requirements [cc : CatalogCycle]) : (Listof Requirement)
  (append
   computing-common-requirements
   (list (req  "csc225")
         (req  "csc305")
         (req  "csc308")
         (req  "csc309")
         (list "csc348" passed-discrete?)
         (req "csc349")
         (req  "csc402")
         (req  "csc405")
         (req  "csc406")
         (req  "csc430")
         (req  "csc484")
         
         (list '(upper-level-se-TE)
               (passed-upper-level-se-technical-elective? cc))
         (list '(special-problems/se-TE)
               (or!/req got-special-problems-credit?
                        (passed-se-technical-elective? cc)))
         (list '(se-TE/123)
               (or!/req passed-123?
                        (passed-se-technical-elective? cc))))
))

(define 2017-2019-se-requirements : (Listof Requirement)
  (append
   (common-se-requirements "2017-2019")
   (list (req "csc300")
         (req "csc491")
         (req "csc492"))
   ;; 20 TE units minus upper-level (above) minus special problems
   (make-TE-requirements "se" (passed-se-technical-elective? "2017-2019") 3)))
;; count TEs

(define 2019-2020-se-requirements : (Listof Requirement)
  (append
   (common-se-requirements "2019-2020")
   (all-of-these
    (ann "2019-2020" CatalogCycle)
    '(ethics
      "csc365"))
   ;; 16 TE units minus upper-level minus special problems
   (make-TE-requirements "se" (passed-se-technical-elective? "2019-2020") 2)))
;; count TEs



(define (common-cpe-requirements [cc : CatalogCycle]) : (Listof Requirement)
  (append
   computing-common-requirements
   (append
    (all-of-these
     cc
     `("cpe100"
       "cpe133"
       "cpe233"
       "cpe350"
       "cpe450"
       "csc453"
       "cpe464"
       "csc348"
       ;(req "ee211")
       ;(req "ee241")
       cpe-TE/400
       cpe-TE/123
       )))
   (make-TE-requirements "cpe" (passed-cpe-technical-elective? cc) 2)))

(define 2017-2019-cpe-requirements
  (append
   (common-cpe-requirements "2017-2019")
   (list (req "cpe315")
         (req "cpe329")
         (req "cpe461")
         (req "cpe462")
         )))

(define 2019-2020-cpe-requirements
  (append
   (common-cpe-requirements "2019-2020")
   (all-of-these
    (ann "2019-2020" CatalogCycle)
    '(cpe-arch
      microcon
      cpe-sp-1
      cpe-sp-2))
   ;; ee211 .. omitting ee things for now
   ))

;; waiting for new te's
(define 2020-2021-cpe-requirements
  (append
   (common-cpe-requirements "2020-2021")
   (all-of-these
    (ann "2020-2021" CatalogCycle)
    '(cpe-arch
      microcon
      cpe-sp-1
      cpe-sp-2))))

(define 2020-2021-ee-requirements
  (list (req "cpe133")
        (req "cpe233")
        (req "ee111")
        (req "ee151")
        ;; slight approximation, should be (or (and ... ...) (and ... ...))
        (list '(circuits) (passed-one-of '("ee112" "ee113")))
        (list '(circuits-lab) (passed-one-of '("ee143" "ime156")))
        ;; check to make sure IME courses aren't filtered out
        (req "ee211")
        (req "ee241")
        (req "ee212")
        (req "ee242")
        (req "ee228")
        (req "ee255")
        (req "ee295")
        (req "ee302")
        (req "ee342")
        (req "ee306")
        (req "ee346")
        (req "ee307")
        (req "ee347")
        (req "ee308")
        (req "ee348")
        (req "ee314")
        (req "ee328")
        (req "ee368")
        (list '(mozzz) (passed-one-of '("cpe329" "ee336")))
        (req "ee335")
        (req "ee375")
        (req "ee402")
        (req "ee409")
        (req "ee449")
        (req "ee460")
        ;; same approx
        (list '(ee-sp-1) (passed-one-of '("ee461" "ee463")))
        (list '(ee-sp-2) (passed-one-of '("ee462" "ee464")))
        ;; this could be tighter... it fails to really check the 11 unit requirement.
        ;; I think this is good enough for forecasting.
        (list '(ee-te-1) passed-ee-te-lec-lab-req?)
        (list '(ee-te-2) passed-ee-te-lec-lab-req?)
        (list '(ee-te-3) passed-ee-te-open-req?)
        ))


(define-type LAC (List Any CatalogCycle))


(define program-requirements
  : (Immutable-HashTable LAC (Listof Requirement))
  (make-immutable-hash
   (ann 
   (append
    (ann (list
          (let ([cc : CatalogCycle "2017-2019"])
            (cons
             (list '(CSC) cc)
             (append
              (common-csc-requirements cc)
              (all-of-these
               cc
               '("csc431"
                 "csc300"
                 "csc491"
                 "csc492"))
              ;; 24 TE units minus upper-level (above) minus special-problems = 4 courses:
              (make-csc-te-reqs cc 4))))
          (let ([cc : CatalogCycle "2019-2020"])
            (cons (list '(CSC) cc)
                  (append
                   (common-csc-requirements cc)
                   (all-of-these
                    cc
                    '(ethics
                      csc-sp-1
                      csc-sp-2))
                   (make-csc-te-reqs cc 5))))
          ;; waiting on TE listing
          #;(let ([cc : CatalogCycle "2020-2021"])
              (cons (list '(CSC) cc)
                    (append
                     ;; technically 431 is still required; in reality... it's not?
                     (common-csc-requirements cc)
                     (all-of-these
                      cc
                      '(ethics
                        csc-sp-1
                        csc-sp-2))
                     (make-csc-te-reqs cc 5)))))
         (Listof (Pairof LAC (Listof Requirement))))
    (ann (list
          (cons (list '(SE) (ann "2017-2019" CatalogCycle)) 2017-2019-se-requirements)
          (cons (list '(SE) (ann "2019-2020" CatalogCycle)) 2019-2020-se-requirements)
          #;(cons (list '(SE) (ann "2020-2021" CatalogCycle)) 2020-sereqs)
          (cons (list '(CPE) (ann "2017-2019" CatalogCycle)) 2017-2019-cpe-requirements)
          (cons (list '(CPE) (ann "2019-2020" CatalogCycle)) 2019-2020-cpe-requirements)
          #;(cons (list '(CPE) (ann "2020-2021" CatalogCycle)) 2020-2021-cpe-requirements))
         (Listof (Pairof LAC (Listof Requirement)))))
   (Listof (Pairof LAC (Listof Requirement))))))

(for ([tups (hash->list program-requirements)])
  (ensure-distinct-names (cdr tups)))

;; for use in checking for specific courses:
(define pass-requirement pass/req)
(define pass-with-c-requirement passc/req)
(define pass-in-qtr-requirement pass-in-qtr/req)
(define pass-with-c-in-qtr-requirement pass/c-in-qtr/req)
(define took-in-qtr-requirement took-in-qtr/req)

;; return a list of the names of the student's unsatisfied requirements
(define (missing-requirements [rs : (Listof Requirement)]
                              [gr : (Listof Grade-Record)]) : (Listof ReqName)
  (define-values (_ missing)
    (for/fold : (values (Listof (Listof Grade-Record))
                        (Listof ReqName))
      ([grs : (Listof (Listof Grade-Record)) (list gr)]
       [missing : (Listof ReqName) '()])
      ([requirement (in-list rs)])
      (define ways-of-satisfying-req
        (remove-duplicates (apply append (map (second requirement) grs))))
      (match ways-of-satisfying-req
        ;; failed this requirement; put it on the list and keep going
        ['() (values grs (cons (first requirement) missing))]
        [ways (values ways missing)])))
  (reverse missing))

(module+ test
  (require typed/rackunit)

  (check-equal? (got-special-problems-credit?
                 '((2178 "csc101" 4 "A")
                   (2178 "csc400" 1 "A")
                   (2178 "csc101" 4 "A")
                   (2178 "csc101" 4 "A")
                   (2178 "csc490" 1 "A")
                   (2178 "csc490" 1 "A")
                   (2178 "csc101" 4 "A")))
                '())

  (check-equal? (got-special-problems-credit?
                 '((2178 "csc101" 4 "A")
                   (2178 "csc400" 1 "A")
                   (2178 "csc102" 4 "A")
                   (2178 "csc103" 4 "A")
                   (2178 "cpe400" 2 "A")
                   (2178 "csc490" 1 "A")
                   (2178 "csc104" 4 "A")))
                '(((2178 "csc101" 4 "A")
                   (2178 "csc102" 4 "A")
                   (2178 "csc103" 4 "A")
                   (2178 "csc104" 4 "A"))))
  
  (check-equal?
   ((or!/req (pass/req "cpe100")
             (pass/req "csc101"))
    '((2178 "csc400" 4 "A")
      (2178 "csc400" 4 "A")
      (2178 "csc400" 4 "A")
      (2178 "csc400" 4 "A")))
   '())

  (check-equal?
   ((or!/req (pass/req "cpe100")
             (pass/req "csc101"))
    '((2178 "csc400" 4 "A")
      (2178 "cpe100" 4 "A")
      (2178 "csc400" 4 "A")
      (2178 "csc400" 4 "A")))
   '(((2178 "csc400" 4 "A")
      (2178 "csc400" 4 "A")
      (2178 "csc400" 4 "A"))))

  (check-equal?
   ((pass/req "cpe100")
    '((2178 "csc400" 4 "A")
      (2178 "cpe100" 4 "D")
      (2178 "csc400" 4 "A")
      (2178 "csc400" 4 "A")))
   '(((2178 "csc400" 4 "A")
      (2178 "csc400" 4 "A")
      (2178 "csc400" 4 "A"))))

  (check-equal?
   ((passc/req "cpe100")
    '((2178 "csc400" 4 "A")
      (2178 "cpe100" 4 "D")
      (2178 "csc400" 4 "A")
      (2178 "csc400" 4 "A")))
   '())

  (check-equal?
   ((pass-in-qtr/req "cpe100" 2178)
    '((2178 "csc400" 4 "A")
      (2178 "cpe100" 4 "D")
      (2178 "csc400" 4 "A")
      (2178 "csc400" 4 "A")))
   '(((2178 "csc400" 4 "A")
      (2178 "csc400" 4 "A")
      (2178 "csc400" 4 "A"))))

  (check-equal?
   ((pass/c-in-qtr/req "cpe100" 2178)
    '((2178 "csc400" 4 "A")
      (2178 "cpe100" 4 "A")
      (2178 "csc400" 4 "A")
      (2178 "csc400" 4 "A")))
   '(((2178 "csc400" 4 "A")
      (2178 "csc400" 4 "A")
      (2178 "csc400" 4 "A"))))
  
  (check-equal?
   ((pass/c-in-qtr/req "cpe100" 2178)
    '((2178 "csc400" 4 "A")
      (2178 "cpe100" 4 "A")
      (2178 "csc400" 4 "A")
      (2178 "csc400" 4 "A")))
   '(((2178 "csc400" 4 "A")
      (2178 "csc400" 4 "A")
      (2178 "csc400" 4 "A"))))

  (check-equal?
   ((pass-in-qtr/req "cpe100" 2174)
    '((2178 "csc400" 4 "A")
      (2178 "cpe100" 4 "A")
      (2178 "csc400" 4 "A")
      (2178 "csc400" 4 "A")))
   '())

  (check-equal?
   ((or!/req (pass/req "cpe100")
             (pass/req "csc101"))
    '((2178 "csc400" 4 "A")
      (2178 "csc101" 4 "A")
      (2178 "csc400" 4 "A")
      (2178 "csc400" 4 "A")))
   '(((2178 "csc400" 4 "A")
      (2178 "csc400" 4 "A")
      (2178 "csc400" 4 "A")))
   "zz1")
  
  (check-equal?
   (passed-101? '((2138 "cpe100" 4 "CR")
                  (2138 "csc123" 4 "A")
                  (2142 "csc102" 4 "A")))
   (list '((2138 "cpe100" 4 "CR")
           (2138 "csc123" 4 "A")
           (2142 "csc102" 4 "A"))))

  (check-equal?
   ((passed-cpe-technical-elective? "2017-2019")
    '((2178 "csc100" 4 "A")
      (2178 "csc101" 4 "A")
      (2178 "csc100" 4 "A")
      (2178 "csc100" 4 "A")))
   '())

  (check-equal?
   ((passed-cpe-technical-elective? "2017-2019")
    '((2178 "csc100" 4 "A")
      (2178 "csc101" 4 "A")
      (2178 "csc530" 4 "A")
      (2178 "csc100" 4 "A")))
   '(((2178 "csc100" 4 "A")
      (2178 "csc101" 4 "A")
      (2178 "csc100" 4 "A"))))

  ;; not a part of this file any more...
  #;((check-equal? (apply
                 +
                 (map
                  (inst second Any Real Any)
                  (map
                   requirement-work
                   (map (inst first String Any) csc-requirements))))
                88)

  (check-equal? (apply
                 +
                 (map
                  (inst second Any Real Any)
                  (map
                   requirement-work
                   (map (inst first String Any) cpe-requirements))))
                ;; REGRESSION:
                55))

  (define 2017-2019-csc-requirements
    (hash-ref program-requirements '((CSC) "2017-2019")))
  (check-equal? (missing-requirements 2017-2019-csc-requirements '())
                (map (inst first ReqName Any) 2017-2019-csc-requirements))

  
  (check-equal? (missing-requirements 2017-2019-cpe-requirements '())
                (map (inst first ReqName Any) 2017-2019-cpe-requirements))

  (check-equal? (missing-requirements 2017-2019-csc-requirements
                                      '((2178 "csc202" 4 "A")))
                (remove* '("csc101" "csc202")
                         (map (inst first ReqName Any) 2017-2019-csc-requirements))))
