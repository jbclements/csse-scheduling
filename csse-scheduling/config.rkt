#lang typed/racket

(provide config-env)

(require racket/runtime-path)
(define-runtime-path here ".")

(define-type ConfigContent (Listof (Pairof Symbol (U Natural String))))
(define-predicate config-content? ConfigContent)

(define config-sexp : ConfigContent
  (let ()
    (define pre-content (file->value (build-path here "config-prvt.rktd")))
    (cond [(config-content? pre-content) pre-content]
          [else (error 'config-sexp "expected config content, got ~e"
                       pre-content)])))

(define (config-env [tag : Symbol])
  (match (assoc tag config-sexp)
    [(cons _ val) val]
    [#f (error 'config-env "no config var with tag ~v" tag)]))

