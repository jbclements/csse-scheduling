#lang typed/racket

(provide config-env)

(require racket/runtime-path)
(define-runtime-path here ".")

(define config-sexp
  (cast (file->value (build-path here "config-prvt.rktd"))
        (Listof (Pairof Symbol (U Natural String)))))

(define (config-env [tag : Symbol])
  (match (assoc tag config-sexp)
    [(cons _ val) val]
    [#f (error 'config-env "no config var with tag ~v")]))

