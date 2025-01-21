#lang typed/racket

(provide db-username
         db-password
         db-maybe-port)

(require racket/runtime-path)
(define-runtime-path here ".")

(define private-credentials-file
  (build-path here "credentials-prvt.rktd"))

(define credentials-sexp
  (cond [(file-exists? private-credentials-file)
         (cast (file->value private-credentials-file)
               (Listof (Pairof Symbol (U Natural String))))]
        [else
         (eprintf "can't find private credentials file, no cache refresh possible\n")
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

