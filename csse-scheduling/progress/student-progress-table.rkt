#lang typed/racket

;; looks like this file is where requirements and students come together.


(require "student-progress.rkt"
         csse-scheduling/qtr-math
         "degree-requirements.rkt")

(define next-fall-year 2018)

(provide major-requirements
         major-requirement-names
         max-num-requirements
         student->bools
         student->unmet-requirements
         first-has-earlier-false?
         grad-years-possible

         and/p
         not/p
         or/p
         expected-to-graduate-by
         entered-on-or-before
         has-major-in
         satisfies)

(define (major-requirements [major : String]) : (Listof Requirement)
  (match major
    ["CSC" csc-requirements]
    ["CPE" cpe-requirements]
    ["SE"  se-requirements]
    [other (raise-argument-error 'major-requirements
                                 "string that's one of: CSC, CPE, or SE"
                                 0 major)]))

(define max-num-requirements
  (apply max (map (inst length Requirement)
                  (list csc-requirements
                        se-requirements
                        cpe-requirements))))

;; given a major, return the names of the requirements
(define (major-requirement-names [major : String]) : (Listof String)
  (map (inst first String Any) (major-requirements major)))


;; requirements no longer produce booleans, so this is all commented out for now:

;; given a student, produce a list of booleans representing their
;; satisfaction of the requirements. A #t indicates the requirement has been met.
(define (student->bools [student : Student]) : (Listof Boolean)
  (define requirements (major-requirements (Student-major student)))
  (define needed (student->unmet-requirements student))
  (for/list ([req-name (in-list (map (inst first String) requirements))])
    (not (member req-name needed))))

;; given a student, produce a list of their unmet requirement names
(define (student->unmet-requirements [student : Student]) : (Listof String)
  (define requirements (major-requirements (Student-major student)))
  (missing-requirements requirements (Student-grades student)))

(define (first-has-earlier-false? [lob1 : (Listof Boolean)]
                                  [lob2 : (Listof Boolean)]) : Boolean
  (match (list lob1 lob2)
    [(list '() '()) #f]
    [(list (cons #f rest1)
           (cons #t rest2))
     #t]
    [(list (cons #t rest1)
           (cons #f rest2))
     #f]
    [(list (cons _ rest1)
           (cons _ rest2))
     (first-has-earlier-false? rest1 rest2)]
    [_ (error 'lists-of-different-lengths)]))

;; if the student graduated this year, what category would they fall into?
(define (grad-years-possible [entry-qtr : Qtr]) : String
  (match (- next-fall-year (qtr->fall-year entry-qtr))
    [1 "1 year"]
    [2 "2 years"]
    [3 "3 years"]
    [4 "4 years"]
    [(or 5 6) "5-6 years"]
    [other
     (cond [(<= 7 other) "7+ years"]
           [else (raise-argument-error
                  'grad-years-possible
                  "plausible entry quarter"
                  0 entry-qtr)])]))


;; STUDENT REQUIREMENT COMBINATORS

(: and/p ((Student -> Boolean) * -> (Student -> Boolean)))
(define (and/p . preds)
  (λ ([s : Student])
    (for/and : Boolean ([pred : (Student -> Boolean) (in-list preds)])
      (pred s))))

(: or/p ((Student -> Boolean) * -> (Student -> Boolean)))
(define (or/p . preds)
  (λ ([s : Student])
    (for/or : Boolean ([pred : (Student -> Boolean) (in-list preds)])
      (pred s))))

(define (not/p [pred : (Student -> Boolean)]) : (Student -> Boolean)
  (λ ([s : Student]) (not (pred s))))

(define (expected-to-graduate-by [qtr : Qtr]) : (Student -> Boolean)
  (λ ([s : Student])
    (match (Student-expected-graduation-qtr s)
      ['future #f]
      [(? natural? n) (<= n qtr)])))

(define (entered-on-or-before [qtr : Qtr]) : (Student -> Boolean)
  (λ ([s : Student])
    (define eqtr (Student-entry-qtr s))
    (and (not (equal? eqtr 'pre-poly)) (<= eqtr qtr))))

(define (has-major-in [lom : (Listof String)]) : (Student -> Boolean)
  (λ ([s : Student])
    (and (member (Student-major s) lom)
         #t)))

(define (satisfies [req : ReqFun]) : (Student -> Boolean)
  (λ ([s : Student])
    (not (empty? (req (Student-grades s))))))


(module+ test
  (require typed/rackunit)

  (check-equal? (first-has-earlier-false? '(#t #t #f #t #t)
                                          '(#t #t #t #f #t))
                #t)
  (check-equal? (first-has-earlier-false? '(#t #t #t #f #t)
                                          '(#t #t #f #t #t))
                #f)
  )

(module+ main

  (define students (get-students "2184-1"))
  (define blork
    (write-to-file
     #:exists 'truncate
     (map (λ ([s : Student])
            (list (Student-id s) (student->unmet-requirements s)))
          students)
                   "/tmp/abc.rktd"))

  )