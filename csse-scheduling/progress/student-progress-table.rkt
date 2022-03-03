#lang typed/racket

;; looks like this file is where requirements and students come together.

;; FIXME "student-progress-table.rkt" is a terrible name for this file.

(require "../types.rkt"
         "student-progress.rkt"
         csse-scheduling/qtr-math
         "degree-requirements.rkt")

(provide major-requirements
         major-requirement-names
         max-num-requirements
         student->bools
         student->unmet-requirements
         first-has-earlier-false?

         and/p
         not/p
         or/p
         expected-to-graduate-by
         entered-on-or-before
         has-major-in
         satisfies)


(define (major-requirements [major : String] [cc : CatalogCycle])
  : (Listof Requirement)
  (define key (list (list (string->symbol major)) cc))
  (hash-ref program-requirements key
            (λ () (error "program requirements table doesn't have an entry for ~e"
                         key))))

(define max-num-requirements
  (apply max (map (inst length Requirement)
                  (hash-values program-requirements))))

;; given a major, return the names of the requirements
(define (major-requirement-names [major : String] [cc : CatalogCycle])
  : (Listof ReqName)
  (map (inst first ReqName Any) (major-requirements major cc)))


;; given a student, produce a list of booleans representing their
;; satisfaction of the requirements. A #t indicates the requirement has been met.
(define (student->bools [student : Student] [cc : CatalogCycle]) : (Listof Boolean)
  (define requirements (major-requirements (Student-major student) cc))
  (define needed (student->unmet-requirements student cc))
  (for/list ([req-name (in-list (map (inst first ReqName) requirements))])
    (not (member req-name needed))))

;; given a student, produce a list of their unmet requirement names
(define (student->unmet-requirements [student : Student] [cc : CatalogCycle])
  : (Listof ReqName)
  (define requirements (major-requirements (Student-major student) cc))
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
            (list (Student-id s) (student->unmet-requirements s "2017-2019")))
          students)
                   "/tmp/abc.rktd"))

  )