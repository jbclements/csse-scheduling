#lang racket

;; right now, this just parses the list of CPE-owned courses.

(require racket/runtime-path
         csv-reading
         sugar
         "discard-bom.rkt"
         csse-scheduling/canonicalize)

(provide 2022-cpe-courses)

(define-runtime-path here ".")

(define cc "2022-2026")

(define table
  (call-with-input-file (build-path here "CPE-housed courses.csv") 
    (λ (port)
      (csv->list (discard-bom port)))))

(when (not (equal? '(1) (remove-duplicates (map length table))))
  (error 'table-shape
         "expected a single column"))

(define cells (map first table))

(define groups
  (map
   (λ (g)
     (cond [(and (pair? g) (equal? (first g) ""))
            (rest g)]
           [else g]))
   (slicef-at cells (λ (c) (equal? c "")))))

(define titles
  (map first groups))

;; NB ligatures or other nonstandard characters in here, sigh...
;; if this fails, make sure that the new value of titles doesn't
;; contain any courses, and just reset it. This is essentially just
;; a regression test.
(unless (equal? titles
                '("CPE-housed Courses"
                  "Orientation"
                  "Architecture, Parallel, and Distributed Computing"
                  "Embedded Systems"
                  "Computer Networks"
                  "Robotics"
                  "Ethical Computing, Privacy, and Security"
                  "Design"
                  "Graduate Courses"
                  "CPE Special Topics / Project Courses "))
  (error 'title-check "unexpected titles"))



(define (parse-line s)
  (match-define (list _ pre num post) (regexp-match #px"^(.*)([[:digit:]]{3})(.*)$" s))
  (define subjects
    (match (string-trim pre)
      [(or "CPE" "CSC" "EE") (list (string-trim pre))]
      [(regexp #px"^([A-Z]{2,3})\\(([A-Z]{2,3})\\)" (list _ a b))
       (list a b)]      
      [other (error "unexpected prefix: ~e" other)]))
  (list subjects (string->number (string-trim num))))

(define (my-canonicalize subjects num)
  (remove-duplicates
   (map (λ (subject) (canonicalize cc subject num))
        subjects)))

(define courses
  (map (λ (pr) (apply my-canonicalize pr))
       (map parse-line (apply append (map rest groups)))))

(unless (equal? '(1) (remove-duplicates (map length courses)))
  (error 'courses "two courses listed as one. add info here"))

(define 2022-cpe-courses
  (apply append courses))