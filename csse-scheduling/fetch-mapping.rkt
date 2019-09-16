#lang racket

(provide course-mappings
         courses-we-schedule/db
         2017-course-configurations
         2019-course-configurations
         student-grades-cache
         latest-student-grades-cache
         majors-cache
         2188-1-student-grades
         2188-1-majors)


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
(define (cache-query db-name name query-fn)
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
                            #:database db-name
                            #:password db-password))
      (query-fn conn)))))

;; fetch the course mappings from the database
(define course-mappings
  (cache-query
   "scheduling"
   "course-mappings.withcache"
   (λ (conn)
     (query-rows
      conn
      (~a "SELECT * FROM course_mappings")))))

(define courses-we-schedule/db
  (cache-query
   "scheduling"
   "courses-we-schedule.withcache"
   (λ (conn)
     (define ids
       (query-list
        conn
        (~a "SELECT id FROM our_courses")))
     (list->set ids))))

(define (configurations-cache base-year)
  (cache-query
   "scheduling"
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

;; extending this to handle the ad-hoc per-student data as well...

(define (student-grades-cache version)
  (cache-query
   "csseprogress"
   (~a version"-student-grades.withcache")
   (λ (conn)
     (map
      vector->list
      (query-rows
       conn
       (~a "SELECT id,qtr,course,units_earned,grade FROM course_grade"
           " WHERE version=$1;")
       version)))))

(define (latest-student-grades-cache)
  (cache-query
   "csseprogress"
   "latest-student-grades.withcache"
   (λ (conn)
     (map
      vector->list
      (query-rows
       conn
       (~a "SELECT id,qtr,course,units_earned,grade FROM latest_course_grade;"))))))

(define (majors-cache version)
  (cache-query
   "csseprogress"
   (~a version"-majors.withcache")
   (λ (conn)
     (query-rows
      conn
      (~a "SELECT m.id,major,e.qtr,g.qtr"
          " FROM ((majors m LEFT JOIN entry_qtr e ON m.id=e.id AND m.version=e.version)"
          "       LEFT JOIN grad_qtr g ON m.id = g.id AND m.version = g.version)"
          " WHERE m.version=$1"
          " and (m.id, m.version) NOT IN (SELECT id,version FROM csgrad);")
      version))))

(define 2188-1-student-grades (student-grades-cache "2188-1"))
(define 2188-1-majors (majors-cache "2188-1"))

