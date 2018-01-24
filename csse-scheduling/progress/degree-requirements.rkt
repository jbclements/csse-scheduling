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
         make-class-requirement)

(define-type Qtr Natural)
(define-type Course-Id String)
(define-type Grade String)
(define-type Units Real)
(define-type Grade-Record (List Qtr Course-Id Units Grade))

(define gr-course second)
(define gr-units third)
(define gr-grade fourth)

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

;; combine two requirement-funs with an 'and, assuming
;; the requirements must be satisfied by different course sets
(define (and/req [rf1 : ReqFun] [rf2 : ReqFun]) : ReqFun
  (λ ([g : (Listof Grade-Record)])
    (define ways1 (rf1 g))
    (remove-duplicates
     (apply
      append
      (for/list : (Listof (Listof Grade-Record))([way (in-list ways1)])
        (rf2 way))))))

;; ooh, what about this?

;; has the student passed the "bigger projects"/OO class?
(define passed-bigger-projects? : ReqFun
  (or/req (passc/req "csc102")
          (passc/req "csc203")))

;; has the student passed the data structures class?
(define passed-data-structures? : ReqFun
  (or/req (passc/req "csc202")
          (passc/req "csc103")))

;; passed-101 : has the student passed 101 or taken a later class
;; indicating that they didn't need it?
(define passed-101?
  (or/req (passc/req "csc101")
          (or/req passed-bigger-projects?
                  passed-data-structures?)))

;; did this csc student pass the SE requirement? (307 OR 308+309)
(define passed-csc-se-req? : ReqFun
  (or/req (pass/req "csc307")
          (and/req (pass/req "csc308")
                   (pass/req "csc309"))))

;; did this csc student pass the 'discrete' requirement (141 or 348)?
(define passed-discrete? : ReqFun
  (or/req (pass/req "csc141")
          (pass/req "csc348")))

;; did this student get 4 units total from csc400, cpe400, or csc490.
(define got-special-problems-credit? : ReqFun
  (λ ([g : (Listof Grade-Record)])
    (<= 4 (+ (total-passed-units g "csc400")
             (total-passed-units g "cpe400")
             (total-passed-units g "csc490")))))

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

;; represents the requirement for a technical elective
(define passed-technical-elective? : ReqFun
  (apply or/req (map pass/req 2017-9-tes)))

;; represents the requirement for an upper-level technical elective
(define passed-upper-level-technical-elective? : ReqFun
  (apply or/req (map pass/req 2017-9-upper-level-tes)))


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
           
           )
     ;; 24 TE units minus upper-level (above) plus 123 = 6 classes:
     (for/list ([i (in-range 6)])
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

(module+ test
  (require typed/rackunit)

  
  (check-true
   (passed-101? '((2138 "cpe100" "CR")
                  (2138 "csc123" "A")
                  (2142 "csc102" "A")))))