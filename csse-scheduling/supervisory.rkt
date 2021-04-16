#lang typed/racket

;; which courses & groups are supervisory?

(require "types.rkt"
         "scheduled-by-csse-dept.rkt"
         "group-courses.rkt")

(provide supervisory?)

;; could be checked against FAD...

;; is this course or group supervisory?
(define (supervisory? [course : Course-Or-Group]) : Boolean
  (match course
    [(? string? course)
     (set-member? supervisory-courses course)]
    [(list sym)
     (match (assoc course simple-group-courses)
       [(cons _ (list course-ids ...))
        ;; they should all be members, or none
        (define results
          (remove-duplicates
           (map (λ ([id : Course-Id])
                  (set-member? supervisory-courses id)) course-ids)))
        (match results
          [(list #t) #t]
          [(list #f) #f]
          [other (error 'supervisory?
                        "internal error: group ~v contains both supervisory\
 and non-supervisory courses.")])]
       [#f
        (match )
        (error 'supervisory?
                  "unknown group: ~v" course)])]))

(module+ test
  (require typed/rackunit)

  (check-equal? (supervisory? "csc400") #t)
  (check-equal? (supervisory? "csc123") #f)
  (check-equal? (supervisory? '(cpe-sp-1)) #t)
  (check-equal? (supervisory? '(microcon)) #f))

