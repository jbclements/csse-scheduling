#lang typed/racket

(provide db-username
         db-password)

(require racket/runtime-path)
(define-runtime-path here ".")

(define credentials-sexp
  (cast (file->value (build-path here "credentials-prvt.rktd"))
        (Listof (Pairof Symbol String))))

(define db-username (cdr (cast (assoc 'username credentials-sexp)
                               (Pairof Symbol String))))
(define db-password (cdr (cast (assoc 'password credentials-sexp)
                               (Pairof Symbol String))))

