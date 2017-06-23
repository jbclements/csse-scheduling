#lang racket

(provide course-mappings)
(require db
         with-cache
         "credentials.rkt")

(define week-seconds (* 7 86400))

;; fetch the course mappings from the database
(define course-mappings
  (parameterize ([*current-cache-keys*
                  (list (λ ()
                          (floor (/ (current-seconds)
                                    week-seconds))))])
  (with-cache (cachefile "course-mappings.rktd")
      (λ ()
        (printf "refreshing cache...\n")
        (define conn
          (postgresql-connect #:port 13432
                              #:user db-username
                              #:database "scheduling"
                              #:password db-password))
        (define rows
          (query-rows
           conn
           (~a "SELECT * FROM course_mappings")))
        rows))))