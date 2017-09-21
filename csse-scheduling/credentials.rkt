#lang typed/racket

(provide db-username
         db-password
         db-maybe-port)

(require racket/runtime-path)
(define-runtime-path here ".")

(define credentials-sexp
  (cast (file->value (build-path here "credentials-prvt.rktd"))
        (Listof (Pairof Symbol String))))


(define db-username : String
  (match (assoc 'username credentials-sexp)
    [(cons _ (? string? username))
     username]
    [#f (error 'db-username
               "credentials file missing binding for 'username")]))

(define db-password : String
  (match (assoc 'password credentials-sexp)
    [(cons _ (? string? password))
     password]
    [#f (error 'db-username
               "credentials file missing binding for 'password")]))

(define db-maybe-port : (U #f Natural)
  (match (assoc 'port credentials-sexp)
    [(cons _ (? exact-nonnegative-integer? port))
     port]
    [#f #f]))

