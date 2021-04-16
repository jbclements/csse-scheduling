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
        ;; not in simple-groups, use the table below:
        (hash-ref supervisory-group-table sym)])]))


;; these coures are supervisory
(define supervisory
  '(CSCMSTHESIS))

;; oof, parallel list. Are these supervisory? (no)
;; ugh, actually, it's a bit weird; the cpe-TE/400 requirement *could*
;; be supervisory, but probably isn't, for scheduling purposes.

;; these courses are not supervisory.
(define not-supervisory : (Listof Symbol)
  (append
   (list 'csc-TE-3
         'csc-TE-2
         'cpe-TE-4
         'cpe-TE-2
         'cpe-TE-3
         'se-TE-1
         'se-TE/123
         'csc-TE-1
         'csc-TE-4
         'csc-TE/special-problems
         'cpe-TE-0
         'se-TE-0
         'upper-level-csc-TE
         'cpe-TE-1
         'se-TE-2
         'upper-level-se-TE
         'csc-TE/123
         'special-problems/se-TE
         'csc-TE-0
        
         )
   '(CSCMS5TE
     CSCMSOTE
     CSMINORELEC
     CSCTE)))

(define supervisory-group-table : (Immutable-HashTable Symbol Boolean)
  (make-immutable-hash
   (append
    '((cpe-TE/400 . #f)
     (cpe-TE/123 . #f)
     (cpe-signals . #f)
     (ee-TE-0 . #f)
     (ee-TE-1 . #f)
     (ee-TE-2 . #f))
    (map (λ ([s : Symbol]) (cons s #f))
         not-supervisory)
    (map (λ ([s : Symbol]) (cons s #t))
         supervisory))))


(module+ test
  (require typed/rackunit)

  (check-equal? (supervisory? "csc400") #t)
  (check-equal? (supervisory? "csc123") #f)
  (check-equal? (supervisory? '(cpe-sp-1)) #t)
  (check-equal? (supervisory? '(microcon)) #f)
  (check-equal? (supervisory? '(cpe-signals)) #f)

  ;; check that supervisory-group-table has an entry for everything
  ;; in ... um ... that other table
  (require (only-in "progress/degree-requirements.rkt" course-group-names))

  (check-equal? (list->set course-group-names)
                (set-union
                 (list->set
                  (map (inst car Symbol)
                       (map (inst car (List Symbol)) simple-group-courses)))
                 (list->set (hash-keys supervisory-group-table))))
  

  )
