#lang racket

(provide course-mappings
         courses-we-schedule/db
         2017-course-configurations
         2019-course-configurations)

(require db
         with-cache
         "credentials.rkt"
         "qtr-math.rkt"
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

(define (configurations-cache base-year)
  (cache-query
   (~a base-year"-course-configurations.withcache")
   (λ (conn)
     (map
      (λ (v) (cons (vector-ref v 0) (vector-ref v 1)))
      (query-rows
       conn
       "SELECT ci.id, ci.configuration
 FROM course_info ci
 WHERE ci.cycle = $1"
       (fall-year->catalog-cycle base-year))))))

(define 2017-course-configurations (configurations-cache 2017))
(define 2019-course-configurations (configurations-cache 2019))