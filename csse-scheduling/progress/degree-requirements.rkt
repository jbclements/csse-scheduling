#lang typed/racket

;; this file contains information related to student requirements
;; for graduation

(require "../canonicalize.rkt")

(provide Qtr
         Course-Id
         Grade
         Grade-Record
         Requirement
         ReqFun
         csc-requirements
         cpe-requirements
         se-requirements
         make-class-requirement
         missing-requirements
         requirement-work)

(define-type Qtr Natural)
(define-type Course-Id String)
(define-type Grade String)
(define-type Units Real)
(define-type Grade-Record (List Qtr Course-Id Units Grade))

(define gr-course : (Grade-Record -> Course-Id) second)
(define gr-units : (Grade-Record -> Units) third)
(define gr-grade : (Grade-Record -> String) fourth)

;; given a list of grades, a course id, and a predicate mapping a grade to
;; a boolean, return a list of lists of grade-records indicating the
;; ways that the requirement could be satisfied.
(define (passed-pred [course-id : Course-Id]
                     [pred : (Grade -> Boolean)]) : ReqFun
  (check-course course-id)
  (λ ([student-grades : (Listof Grade-Record)])
    (define satisfying-rows
      (filter (λ ([gr : Grade-Record])
                (and (equal? (second gr)
                             course-id)
                     (pred (gr-grade gr))))
              student-grades))
    (for/list ([row (in-list satisfying-rows)])
      (remove row student-grades))))

;; is this a canonical course id?
(define (check-course [course-id : String]) : Void
  (unless (canonical-id? course-id)
    (raise-argument-error 'check-course
                          "canonical course id"
                          0 course-id))
  (void))

(define better-than-d-grades : (Listof String)
  '("A" "A-" "B+" "B" "B-" "C+" "C" "C-"))

(define passing-grades : (Listof String)
  '("A" "A-" "B+" "B" "B-" "C+" "C" "C-" "D+" "D" "D-" "CR"))

(define (grade-names->pred [grade-names : (Listof Grade)]) :
  (Grade -> Boolean)
  (λ ([g : Grade]) (not (not (member g grade-names)))))

;; Requirements are interesting; for most requirements, students
;; cannot fulfill multiple requirements using the same class.
;; accordingly, determining whether a collection of requirements
;; is met requires "assigning" grades to certain requirements. To
;; model this, requirements accept a list of grades and return
;; a list of possible ways in which the requirement is satisfied,
;; where a "way" is represented as a list of the class grades not
;; yet "used" by the requirement

;; a ReqFun represents a requirement test; it accepts a list of
;; grades, and returns a list of lists of grade-records,
;; indicating the ways in which that requirement could "use up"
;; the student's grades. The empty list therefore indicates failure.
(define-type ReqFun ((Listof Grade-Record) -> (Listof (Listof Grade-Record))))

(define-type Requirement (List String ReqFun))

;; time for some combinators:

;; given a course id,
;; did the student pass the given course with a grade of C- or better?
(define (passc/req [course-id : Course-Id]) : ReqFun
  (passed-pred course-id (grade-names->pred better-than-d-grades)))

;; given a course id,
;; did the student pass the given course at all?
(define (pass/req [course-id : Course-Id]) : ReqFun
  (passed-pred course-id (grade-names->pred passing-grades)))

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

;; make a "ghost" requirement that checks the requirement but
;; does not deduct the used classes from the list of grades available.
;; useful as e.g. when passing 102 is used as evidence that student
;; has 101 credit
(define (ghost/req [rf : ReqFun]) : ReqFun
  (λ ([g : (Listof Grade-Record)])
    (cond [(not (empty? (rf g))) (list g)]
          [else '()])))

;; has the student passed the "bigger projects"/OO class?
(define passed-bigger-projects? : ReqFun
  (or!/req
   (or!/req (passc/req "csc102")
            (passc/req "csc203"))
   (ghost/req (pass/req "csc357"))))

;; has the student passed the data structures class?
(define passed-data-structures? : ReqFun
  (or!/req
   (or!/req (passc/req "csc202")
            (passc/req "csc103"))
   (ghost/req (pass/req "csc357"))))

;; passed-101 : has the student passed 101 or taken a later class
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

;; did this student get 4 units total from csc400, cpe400, or csc490.
(define got-special-problems-credit? : ReqFun
  (λ ([g : (Listof Grade-Record)])
    (define special-problems-courses '("csc400" "cpe400" "csc490"))
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
                   (2178 "csc104" 4 "A")))))



;; how many units did this student earn from passing the class
(define (total-passed-units [g : (Listof Grade-Record)] [course : Course-Id])
  (check-course course)
  (apply + (map gr-units
                (filter (λ ([g : Grade-Record]) (equal? (gr-course g) course))
                        g))))

;; these classes may be used as technical electives in the 2017-2019 catalog
(define 2017-9-tes
  ;; NB: for accounting purposes, 123 is basically a technical elective:
  '("csc123"
    "csc301"
    "csc305" "csc309" "csc321" "csc323" "csc325" "csc344" "csc365"
    "csc366" "csc369" "csc371" "csc378" "csc400" "csc402" "csc405"
    "csc406" "csc409" "csc410" "csc422" "csc424" "csc429" "csc435"
    "csc436" "csc437" "csc448" "csc454" "csc458" "csc466" "csc468"
    "csc471" "csc473" "csc474" "csc476" "csc477" "csc478" "csc480"
    "csc481" "csc483" "csc484" "csc486" "csc489" "csc490" "csc496"
    "csc508" "csc509" "csc515" "csc521" "csc530" "csc540" "csc550"
    "csc560" "csc564" "csc566" "csc569" "csc570" "csc572" "csc580"
    "csc581" "csc582" "cpe400" "cpe416" "cpe419" "cpe428" "cpe464"
    "cpe465" "cpe482" "cpe485" "cpe488" "data301"))

;; these classes may be used as the upper-level technical elective in the
;; 2017-2019 catalog
(define 2017-9-upper-level-tes
  '("csc325"
    "csc366"  "csc402"  "csc405"  "csc406"  "csc409"  "csc410"
    "csc422"  "csc424"  "csc429"  "csc435"  "csc437"  "csc454"  "csc466"
    "csc468"  "csc473"  "csc474"  "csc476"  "csc477"  "csc478"  "csc481"
    "csc483"  "csc484"  "csc486"  "csc489"  "csc508"  "csc509"  "csc515"
    "csc521"  "csc530"  "csc540"  "csc550"  "csc560"  "csc564"  "csc566"
    "csc572"  "csc580"  "csc581"  "csc582"  "cpe416"  "cpe465"))

;; given a list of classes, return the first success.
;; eliminates lots of garbage
(define (or!/classes/req [courses : (Listof Course-Id)]) : ReqFun
  (λ ([g : (Listof Grade-Record)])
    (or (for/or : (U False (Listof (Listof Grade-Record)))
          ([course (in-list courses)])
          (define try ((pass/req course) g))
          (cond [(empty? try) #f]
                [else try]))
        '())))

;; represents the requirement for a technical elective
(define passed-technical-elective? : ReqFun
  (or!/classes/req 2017-9-tes))

;; represents the requirement for an upper-level technical elective
(define passed-upper-level-technical-elective? : ReqFun
  (or!/classes/req 2017-9-upper-level-tes))


;; CAVEAT: NO WAY TO KNOW IF THE STUDENTS WILL TAKE AN EXTERNAL TE

;; the master list of requirements
(define csc-requirements : (Listof Requirement)
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
     ;; 24 TE units minus upper-level (above) minus special-problems plus 123 = 5 classes:
     (for/list : (Listof Requirement)
       ([i (in-range 5)])
       (list (~a "technical-elective-" i) passed-technical-elective?)))))

(define se-requirements : (Listof Requirement)
  (let ([req (λ ([course-id : Course-Id]) : Requirement
               (list course-id (pass/req course-id)))])
    (list (list "csc101" passed-101?)
          (list "csc202" passed-data-structures?)
          (list "csc203" passed-bigger-projects?)
          (req  "csc225")
          (req  "csc300")
          (req  "csc305")
          (req  "csc308")
          (req  "csc309")
          (list "discrete" passed-discrete?)
          (req  "csc349")
          (req  "csc357")
          (req  "csc402")
          (req  "csc405")
          (req  "csc406")
          (req  "csc430")
          (req  "csc484")
          (req "csc491")
          (req "csc492")
          )
    ))

(define cpe-requirements : (Listof Requirement)
  (let ([req (λ ([course-id : Course-Id]) : Requirement
               (list course-id (pass/req course-id)))])
    (list ; (req "cpe100") not useful for planning, I think
     (list "csc101" passed-101?)
     (list "csc202" passed-data-structures?)
     (list "csc203" passed-bigger-projects?)
     (req "cpe133")
     (req "cpe233")
     (req "cpe315")
     (req "cpe329")
     (req "csc357")
     (req "cpe350")
     (req "cpe450")
     (req "csc453")
     (req "cpe461")
     (req "cpe462")
     (req "cpe464")
     (req "csc348"))))

;; for use in checking for specific classes:
(define make-class-requirement pass/req)

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

;; a work-piece
(define-type WorkPiece (List WorkKind Units))
(define-type WorkKind (U 'ugrad 'grad
                         'csse-senior-project
                         'cpe-senior-project
                         'masters-thesis))

;; what work-piece is required to help a student fill a requirement?
(define (requirement-work [req : String])
  (match req
    [(or "csc491" "csc492") (list 'csse-senior-project 2)]
    [(or "cpe461" "cpe462") (list 'cpe-senior-project 2)]
    ;; we don't have masters' requirements in here yet... :(
    [else (list 'ugrad 4)]))


(module+ test
  (require typed/rackunit)
  
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

  (check-equal? (apply
                 +
                 (map
                  (inst second Any Real Any)
                  (map
                   requirement-work
                   (map (inst first String Any) csc-requirements))))
                88)

  (check-equal? (missing-requirements csc-requirements '())
                (map (inst first String Any) csc-requirements))

  (check-equal? (missing-requirements cpe-requirements '())
                (map (inst first String Any) cpe-requirements))

  (check-equal? (missing-requirements csc-requirements
                                      '((2178 "csc202" 4 "A")))
                (remove* '("csc101" "csc202")
                         (map (inst first String Any) csc-requirements))))