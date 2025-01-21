#lang typed/racket

(provide db-username
         db-password
         db-maybe-port)

(require racket/runtime-path)
(define-runtime-path here ".")

;; when false, don't try to look up credentials
(define enable-credentials? #f)

(define credentials-sexp
  (cond [enable-credentials?
         (cast (file->value (build-path here "credentials-prvt.rktd"))
               (Listof (Pairof Symbol (U Natural String))))]
        [else
         '((username . "bogus")
           (password . "bogus"))
         ]))


(define db-username : String
  (match (assoc 'username credentials-sexp)
    [(cons _ (? string? username))
     username]
    [(cons _ _)
     (error 'db-username "credentials file contained a non-string for a username")]
    [#f (error 'db-username
               "credentials file missing binding for 'username")]))

(define db-password : String
  (match (assoc 'password credentials-sexp)
    [(cons _ (? string? password))
     password]
    [(cons _ _)
     (error 'db-password "credentials file contained a non-string for a password")]
    [#f (error 'db-username
               "credentials file missing binding for 'password")]))

(define db-maybe-port : (U #f Natural)
  (match (assoc 'port credentials-sexp)
    [(cons _ (? exact-nonnegative-integer? port))
     port]
    [#f #f]))

