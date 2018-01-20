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
         negate-requirement)

(define-type Qtr Natural)
(define-type Course-Id String)
(define-type Grade String)
(define-type Grade-Record (List Qtr Course-Id Grade))

;; given a list of grades, a course id, and a predicate mapping a grade to
;; a boolean, return true if the student has a grade in the given
;; course that satisfies the predicate
(define (passed-pred [course-id : Course-Id]
                     [pred : (Grade -> Boolean)])
  (check-course course-id)
  (λ ([student-grades : (Listof Grade-Record)])
    (define satisfying-rows
      (filter (λ ([gr : Grade-Record])
                (and (equal? (second gr)
                             course-id)
                     (pred (third gr))))
              student-grades))
    (not (empty? satisfying-rows))))

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

(define (grade-list->pred [grade-names : (Listof Grade)]) :
  (Grade -> Boolean)
  (λ ([g : Grade]) (not (not (member g grade-names)))))


;; a requirement is a label along with a function mapping a student
;; to a boolean (indicating whether the student has satisfied the
;; requirement)
(define-type ReqFun ((Listof Grade-Record) -> Boolean))
(define-type Requirement (List String ReqFun))

;; time for some combinators:

;; given a course id,
;; did the student pass the given course with a grade of C- or better?
(define (passc/req [course-id : Course-Id]) : ReqFun
  (passed-pred course-id (grade-list->pred better-than-d-grades)))

;; given a course id,
;; did the student pass the given course at all?
(define (pass/req [course-id : Course-Id]) : ReqFun
  (passed-pred course-id (grade-list->pred passing-grades)))

;; combine two requirement-funs with an 'or'
(define (or/req [rf1 : ReqFun] [rf2 : ReqFun])
  (λ ([g : (Listof Grade-Record)])
    (or (rf1 g) (rf2 g))))

;; combine two requirement-funs with an 'and
(define (and/req [rf1 : ReqFun] [rf2 : ReqFun]) : ReqFun
  (λ ([g : (Listof Grade-Record)])
    (and (rf1 g) (rf2 g))))

;; negate a requirement
(define (neg/req [rf1 : ReqFun]) : ReqFun
  (λ ([g : (Listof Grade-Record)])
    (not (rf1 g))))

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



;; the master list of requirements
(define csc-requirements : (Listof Requirement)
  (let ([req (λ ([course-id : Course-Id]) : Requirement
               (list course-id (pass/req course-id)))])
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
          )))

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
(define negate-requirement neg/req)

(module+ test
  (require typed/rackunit)

  
  (check-true
   (passed-101? '((2138 "cpe100" "CR")
                  (2138 "csc123" "A")
                  (2142 "csc102" "A")))))