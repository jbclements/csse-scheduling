#lang racket

(provide course-mappings
         courses-we-schedule/db
         2017-course-configurations)

(require db
         with-cache
         "credentials.rkt"
         racket/runtime-path)

(define-runtime-path here-path ".")

(define week-seconds (* 7 86400))
(define cache-weeks 8)


;; given a cache file name and a function from connection to value,
;; cache it
(define (cache-query name query-fn)
  (parameterize ([*current-cache-keys*
                  (list (λ ()
                          (floor (/ (current-seconds)
                                    (* cache-weeks
                                       week-seconds)))))]
                 [*current-cache-directory*
                  here-path])
  (with-cache (cachefile name)
    (λ ()
      (printf "refreshing cache for ~a\n" name)
      (define conn
        (postgresql-connect #:port (or db-maybe-port 13432)
                            #:user db-username
                            #:database "scheduling"
                            #:password db-password))
      (query-fn conn)))))

;; fetch the course mappings from the database
(define course-mappings
  (cache-query
   "course-mappings.withcache"
   (λ (conn)
     (query-rows
      conn
      (~a "SELECT * FROM course_mappings")))))

(define courses-we-schedule/db
  (cache-query
   "courses-we-schedule.withcache"
   (λ (conn)
     (define ids
       (query-list
        conn
        (~a "SELECT id FROM our_courses")))
     (list->set ids))))

(define 2017-course-configurations
  (cache-query
   "2017-course-configurations.withcache"
   (λ (conn)
     (map
      (λ (v) (cons (vector-ref v 0) (vector-ref v 1)))
      (query-rows
       conn
       "SELECT ci.id, ci.configuration
 FROM (our_courses oc INNER JOIN course_info ci
       ON oc.id = ci.id)
 WHERE ci.cycle = $1"
       "2017-2019")))))