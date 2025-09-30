#lang racket

(provide course-mappings
         courses-we-schedule/db
         cycle-course-configurations
         ee-lab-courses
         student-grades-cache
         latest-student-grades-cache
         majors-cache
         2188-1-student-grades
         2188-1-majors
         ;; just use dept-faculty-offerings instead:
         active-faculty
         offerings
         )


(require db
         with-cache
         "credentials.rkt"
         "qtr-math.rkt"
         racket/runtime-path)

(define-runtime-path here-path ".")

(define week-seconds (* 7 86400))
(define cache-weeks 80)


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

;; FIXME revisit... what is this used for?
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

(define (configurations-cache cycle)
  (cache-query
   "scheduling"
   (~a cycle"-course-configurations.withcache")
   (λ (conn)
     (map
      (λ (v) (cons (vector-ref v 0) (vector-ref v 1)))
      (query-rows
       conn
       "SELECT ci.id, ci.configuration
 FROM course_info ci
 WHERE ci.cycle = $1"
       cycle)))))

(define cycle-course-configurations
  (map (λ (yr)
         (define cycle (fall-year->catalog-cycle yr))
         (cons cycle (configurations-cache cycle)))
       '(2017 2019 2020 2021 2022 2026)))

;; all of the lab-only EE courses:
(define (ee-lab-courses catalog-cycle)
  (cache-query
   "scheduling"
   (~a "ee-lab-courses-"catalog-cycle".withcache")
   (λ (conn)
     (query-list
      conn
      (~a "SELECT ci.id FROM "
          "  (course_mappings cm INNER JOIN course_info ci ON cm.id=ci.id AND cm.cycle=ci.cycle)"
          "  WHERE ci.cycle=$1 AND subject='EE' AND configuration LIKE '0-%-0';")
      catalog-cycle))))


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
          " and (m.id, m.version) NOT IN (SELECT id,version FROM csgrad)"
          " and (m.id, m.version) NOT IN (SELECT id,version FROM eegrad);")
      version))))

(define 2188-1-student-grades (student-grades-cache "2188-1"))
(define 2188-1-majors (majors-cache "2188-1"))

;; extending yet again for the who-does-what queries...

;; actually, adding this was arguably a mistake... should be combined with dept-faculty-offerings.

(define (active-faculty qtr)
  (cache-query
   "fad"
   (~a "active-faculty-" qtr ".withcache")
   (λ (conn)
     (query-list
      conn
      (~a "SELECT id "
          " FROM instructorstatuses"
          " WHERE (homedept='COMPUTER SCIENCE' OR HOMEDEPT='ELECTRICAL ENGINEERING' or homedept='COMPUTER ENGINEERING')"
          " AND qtr >= $1"
          ";")
      qtr))))


(define (offerings last-qtr)
  (cache-query
   "fad"
   (~a "offerings-up-through-" last-qtr ".withcache")
   (λ (conn)
     (map vector->list
          (query-rows
           conn
           (~a "SELECT o.instructor, o.qtr, o.subject, o.num, SUM(o.dwtu) "
               " FROM (offerfacs o JOIN instructorstatuses i"
               "       ON o.instructor = i.id"
               "       AND o.qtr = i.qtr)"
               " WHERE (homedept='COMPUTER SCIENCE' OR homedept='ELECTRICAL ENGINEERING' OR homedept='COMPUTER ENGINEERING')"
               " AND o.qtr <= $1"
               " GROUP BY o.instructor, o.qtr, o.subject, o.num")
           last-qtr)))))

