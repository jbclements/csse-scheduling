#lang typed/racket/base

(require "../qtr-math.rkt"
         "student-progress.rkt")

(provide scc)

;; which catalog should we use to model a particular student?

;; round up to the oldest available catalog for old-timers
(define oldest-catalogs
  (make-immutable-hash
   ;; using 2019 for CSC to get past 431 req.
   '(("CSC" . 2019)
     ("SE" . 2019)
     ("CPE" . 2017)
     ("EE" . 2019))
   ))
(define (scc [student : Student])
  (define entry-qtr (Student-entry-qtr student))
  (cond [(equal? entry-qtr 'pre-poly)
         (error 'scc "which catalog to use for pre-poly students?")]
        [else
         ;; let's just use the catalog they came in under...
         (fall-year->catalog-cycle (max (hash-ref oldest-catalogs (Student-major student))
                                        (qtr->fall-year entry-qtr)))]))
