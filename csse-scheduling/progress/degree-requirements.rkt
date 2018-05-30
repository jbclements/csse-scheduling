#lang typed/racket

;; this file contains information related to student requirements
;; for graduation

(require "../canonicalize.rkt"
         "../course-listings.rkt")

(provide Qtr
         Course-Id
         Grade
         Grade-Record
         Requirement
         ReqFun
         csc-requirements
         cpe-requirements
         se-requirements
         pass-requirement
         pass-in-qtr-requirement
         pass-with-c-in-qtr-requirement
         took-in-qtr-requirement
         missing-requirements)

;; for now, pin the catalog cycle:
(define current-catalog-cycle "2017-2019")

(define-type Qtr Natural)
(define-type Course-Id String)
(define-type Grade String)
(define-type Units Real)
(define-type Grade-Record (List Qtr Course-Id Units Grade))

(define gr-qtr : (Grade-Record -> Qtr) first)
(define gr-course : (Grade-Record -> Course-Id) second)
(define gr-units : (Grade-Record -> Units) third)
(define gr-grade : (Grade-Record -> String) fourth)

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

(define-type Requirement (List String ReqFun))

;; time for some combinators:

;; given a course id,
;; did the student pass the given course with a grade of C- or better?
(define (passc/req [course-id : Course-Id]) : ReqFun
  (passed-pred course-id c-passing-grade?))

;; given a course id,
;; did the student pass the given course at all?
(define (pass/req [course-id : Course-Id]) : ReqFun
  (passed-pred course-id passing-grade?))

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
;; use only when the first possible hit is okay.
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
   (ghost/req (pass/req "csc357"))))

;; has the student passed the data structures course?
(define passed-data-structures? : ReqFun
  (or!/req
   (or!/req (passc/req "csc202")
            (passc/req "csc103"))
   (ghost/req (pass/req "csc357"))))

;; passed-101 : has the student passed 101 or taken a later course
;; indicating that they didn't need it?
(define passed-101?
  (or!/req (passc/req "csc101")
           (ghost/req
            (or!/req passed-bigger-projects?
                     passed-data-structures?))))

;; did this csc student pass the SE requirement? (307 OR 308+309)
(define passed-csc-se-req? : ReqFun
  (or!/req (pass/req "csc307")
          (and/req (pass/req "csc308")
                   (pass/req "csc309"))))

;; did this csc student pass the 'discrete' requirement (141 or 348)?
(define passed-discrete? : ReqFun
  (or!/req (pass/req "csc141")
          (pass/req "csc348")))

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
      (partition (λ ([g : Grade-Record]) (member (gr-course g)
                                                 special-problems-courses))
                 g))
    (define special-topics-units (apply + (map gr-units special-topics-grades)))
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
(define passed-technical-elective? : ReqFun
  (or!/courses/req (hash-ref csc-te-course-table current-catalog-cycle)))

;; represents the requirement for an upper-level technical elective
(define passed-upper-level-technical-elective? : ReqFun
  (or!/courses/req (hash-ref csc-ul-te-course-table current-catalog-cycle)))

;; represents the requirement for a technical elective
(define passed-se-technical-elective? : ReqFun
  (or!/courses/req (hash-ref se-te-course-table current-catalog-cycle)))

;; represents the requirement for an upper-level technical elective
(define passed-upper-level-se-technical-elective? : ReqFun
  (or!/courses/req (hash-ref se-ul-te-course-table current-catalog-cycle)))

;; short-cutting: find a cpe TE and remove it from the list
(define passed-cpe-technical-elective? : ReqFun
  (let ()
    (define cpe-te-courses (hash-ref cpe-te-course-table current-catalog-cycle))
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
  (match (check-duplicates (map (inst first String) reqs))
    [#f reqs]
    [other (error 'ensure-distinct
                  "duplicate requirement name: ~e"
                  other)]))

;; CAVEAT: NO WAY TO KNOW IF THE STUDENTS WILL TAKE AN EXTERNAL TE

;; the master list of requirements
(define csc-requirements : (Listof Requirement)
  (ensure-distinct-names
   (let ([req (λ ([course-id : Course-Id]) : Requirement
                (list course-id (pass/req course-id)))])
     (append
      ;; NB: 123 is *actually* treated like a technical elective...
      (list (list "csc101" passed-101?)
            (list "csc202" passed-data-structures?)
            (list "csc203" passed-bigger-projects?)
            (req  "csc225")
            (req  "csc300")
            (list "SE" passed-csc-se-req?)
            (req  "cpe315")
            (list "discrete" passed-discrete?)
            (req  "csc349")
            (req  "csc357")
            (req "csc430")
            (req "csc431")
            (req "csc445")
            (req "csc453")
            (req "csc491")
            (req "csc492")
           
            (list "upper-level-tech-elect" passed-upper-level-technical-elective?)
            (list "te/special-problems" (or!/req got-special-problems-credit?
                                                 passed-technical-elective?))
           
            )
      ;; 24 TE units minus upper-level (above) minus special-problems plus 123 = 5 courses:
      (for/list : (Listof Requirement)
        ([i (in-range 5)])
        (list (~a "technical-elective-" i) passed-technical-elective?))))))

(define se-requirements : (Listof Requirement)
  (ensure-distinct-names
   (let ([req (λ ([course-id : Course-Id]) : Requirement
                (list course-id (pass/req course-id)))])
     (append
      (list (list "csc101" passed-101?)
            (list "csc202" passed-data-structures?)
            (list "csc203" passed-bigger-projects?)
            (req  "csc225")
            (req  "csc300")
            (req  "csc305")
            (req  "csc308")
            (req  "csc309")
            (list "discrete" passed-discrete?)
            (req "csc349")
            (req "csc357")
            (req  "csc402")
            (req  "csc405")
            (req  "csc406")
            (req  "csc430")
            (req  "csc484")
            (req "csc491")
            (req "csc492")
            (list "se-upper-level-tech-elect"
                  passed-upper-level-se-technical-elective?)
            (list "te/special-problems"
                  (or!/req got-special-problems-credit?
                           passed-se-technical-elective?)))
      ;; 20 TE units minus upper-level (above) minus special problems plus 123
      (for/list : (Listof Requirement)
        ([i (in-range 4)])
        (list (~a "SE-technical-elective-" i)
              passed-se-technical-elective?)))
     )))

(define cpe-requirements : (Listof Requirement)
  (ensure-distinct-names
   (let ([req (λ ([course-id : Course-Id]) : Requirement
               (list course-id (pass/req course-id)))])
    (list
     ; (req "cpe100") not useful for planning?
     (list "csc101" passed-101?)
     (list "csc202" passed-data-structures?)
     (list "csc203" passed-bigger-projects?)
     (req "cpe133") ;; no work for us
     (req "cpe233") ;; no work for us
     (req "cpe315")
     (req "cpe329") ;; no work for us
     (req "csc357")
     (req "cpe350") ;; limited work for us
     (req "cpe450") ;; limited work for us
     (req "csc453")
     (req "cpe461")
     (req "cpe462")
     (req "cpe464")
     (req "csc348")
     (list "cpe-te/400" (or!/req got-4-units-of-400?
                                 passed-cpe-technical-elective?))
     (list "cpe-te1" passed-cpe-technical-elective?)
     (list "cpe-te2" passed-cpe-technical-elective?)
     (list "cpe-te/123" passed-cpe-technical-elective?))
    )))

;; for use in checking for specific courses:
(define pass-requirement pass/req)
(define pass-in-qtr-requirement pass-in-qtr/req)
(define pass-with-c-in-qtr-requirement pass/c-in-qtr/req)
(define took-in-qtr-requirement took-in-qtr/req)

;; return a list of the names of the student's unsatisfied requirements
(define (missing-requirements [rs : (Listof Requirement)]
                              [gr : (Listof Grade-Record)]) : (Listof String)
  (define-values (_ missing)
    (for/fold : (values (Listof (Listof Grade-Record))
                        (Listof String))
      ([grs : (Listof (Listof Grade-Record)) (list gr)]
       [missing : (Listof String) '()])
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
   (passed-cpe-technical-elective?
    '((2178 "csc100" 4 "A")
      (2178 "csc101" 4 "A")
      (2178 "csc100" 4 "A")
      (2178 "csc100" 4 "A")))
   '())

  (check-equal?
   (passed-cpe-technical-elective?
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

  (check-equal? (missing-requirements csc-requirements '())
                (map (inst first String Any) csc-requirements))

  (check-equal? (missing-requirements cpe-requirements '())
                (map (inst first String Any) cpe-requirements))

  (check-equal? (missing-requirements csc-requirements
                                      '((2178 "csc202" 4 "A")))
                (remove* '("csc101" "csc202")
                         (map (inst first String Any) csc-requirements))))