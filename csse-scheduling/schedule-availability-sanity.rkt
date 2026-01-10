#lang typed/racket

;; this file checks that the instructors don't have more units
;; scheduled than they have available

(require "read-schedule.rkt"
         "scheduled-by-csse-dept.rkt"
         "types.rkt"
         "qtr-math.rkt")

(provide spare-capacity-check
         semester-spare-capacity-check
         availability->total-wtus)

;; make sure the two are the same, then return either one of them
(define (=Z a b)
  (unless (equal? a b)
    (error '=Z "these two were supposed to be equal: ~e and ~e"
           a b))
  b)

;; the contract
(define full-time-wtus-per-term 15)

;; tt reductions
(define tt-service-wtus-per-term      3)
(define tt-qtr-advising-wtus-per-term 2)
(define tt-qtr-standard-wtus-per-term (=Z (- full-time-wtus-per-term
                                             tt-service-wtus-per-term
                                             tt-qtr-advising-wtus-per-term)
                                          10))

;; qtrs
(define qtrs-per-year 3)

(define lec-qtr-standard-wtus (=Z (* qtrs-per-year full-time-wtus-per-term)
                                  45))
(define tt-qtr-standard-wtus  (=Z (* qtrs-per-year tt-qtr-standard-wtus-per-term)
                                  30))
(define tt-qtr-first-year-wtus 20)
(define tt-qtr-second-year-wtus 20)

(define absent-wtus 0)

;; semesters

(define semesters-per-year 2)

;; proposed 3-wtu reduction for research
(define tt-scholarship-wtu-reduction 3) ;; per year, not per term
(define tt-sem-advising-wtus-per-term 1)

(define lec-sem-standard-wtus (=Z (* semesters-per-year full-time-wtus-per-term)
                                  30))
(define tt-sem-standard-wtus (=Z (- (* semesters-per-year
                                       (- full-time-wtus-per-term
                                          tt-service-wtus-per-term
                                          tt-sem-advising-wtus-per-term))
                                    tt-sem-advising-wtus-per-term)
                                 21))

(define tt-sem-first-year-wtus 18)
(define tt-sem-second-year-wtus 18)



;; given a schedule and availability, provide warnings about
;; mismatches between the lists of instructors, then provide
;; warnings about instructors that are over their specified
;; availabilities, then return an association list from instructor
;; to spare wtus and total wtus
(define (spare-capacity-check [qtr : CPTN]
                              [scheduled : (Listof InstructorA)]
                              [availability : (Listof (List Symbol Sexp))])
  ((inst spare-capacity-check/helper InstructorA)
   qtr scheduled car instructor-availability availability))

(define (semester-spare-capacity-check [qtr : CPTN]
                                       [scheduled : (Listof InstructorSA)]
                              [availability : (Listof (List Symbol Sexp))])
  ((inst spare-capacity-check/helper InstructorSA)
   qtr scheduled car semester-instructor-availability availability))

(: spare-capacity-check/helper
   (All (IType) (CPTN (Listof IType) (IType -> Symbol)
                      (Symbol IType CatalogCycle Sexp -> (List Symbol Real Real))
                      (Listof (List Symbol Sexp))
                      -> (Listof (List Symbol Real Real)))))
(define (spare-capacity-check/helper qtr scheduled instructor-name instructor-check availability)
  : (Listof (List Symbol Real Real))

  (define this-cycle (qtr->catalog-cycle qtr))
  ;; names from schedule
  (define scheduled-names (map instructor-name scheduled))
  ;; names from availability
  (define available-names (map (inst first Symbol) availability))
  
  (define without-availability
    (set-subtract scheduled-names available-names))
  (when (not (empty? without-availability))
    (printf
     "These instructors have schedules but no listed availability:\n")
    (pretty-display without-availability))

  (define without-schedules
    (set-subtract available-names scheduled-names))
  (when (not (empty? without-schedules))
    (printf
     "These instructors have availability but no listed schedules:\n")
    (pretty-display without-schedules))

  ;; these instructors appear in both lists:
  (define instructors (set-intersect scheduled-names available-names))

  (for/list : (Listof (List Symbol Real Real))
    ([name (in-list instructors)])
    (define irec : IType
      (or (findf (λ ([i : IType]) (equal? (instructor-name i) name)) scheduled)
          (error 'spare-capacity "internal-error 99942")))
    (define i-availability
      (or (findf (λ ([i : (List Symbol Sexp)]) (equal? (first i) name)) availability)
          (error 'spare-capacity "internal error 17192873")))
    ;; cast can't fail by earlier intersection check:
    (instructor-check name
                      irec
                      this-cycle
                      (second i-availability))))

(define (instructor-availability [name : Symbol] [schedule : InstructorA]
                                 [this-cycle : CatalogCycle]
                                 [availability : Sexp])
  : (List Symbol Real Real)
  (define checky (λ ([qtrs : (Listof (U 'f 'w 's))]
                     [wtus : Real])
                   (check-wtus this-cycle name schedule qtrs wtus)))
  (define spare-wtus
    ;; cast must succeed by earlier intersection check:
    (match availability
      ['tt-standard (checky '(f w s) tt-qtr-standard-wtus)]
      ['tt-first-year (checky '(f w s) tt-qtr-first-year-wtus)]
      ['tt-second-year (checky '(f w s) tt-qtr-second-year-wtus)]
      ['lec-standard (checky '(f w s) lec-qtr-standard-wtus)]
      ['absent (checky '(f w s) absent-wtus)]
      [(list 'total (? real? wtus)) (checky '(f w s) wtus)]
      [(list (list 'f (? real? fall-wtus))
             (list 'w (? real? winter-wtus))
             (list 's (? real? spring-wtus)))
       (+ (checky '(f) fall-wtus)
          (checky '(w) winter-wtus)
          (checky '(s) spring-wtus))]
      [(list 'fall-winter (? real? wtus))
       (+ (checky '(f w) wtus)
          (checky '(s) 0))]
      [(list 'winter-spring (? real? wtus))
       (+ (checky '(f) 0)
          (checky '(w s) wtus))]
      [(list 'fall-spring (? real? wtus))
       (+ (checky '(w) 0)
          (checky '(f s) wtus))]
      ;; perform no checks, return zero.
      ['not-ours 0]
      [other (error 'spare-wtus "unrecognized availability format (1): ~e" other)]))
  (define total-wtus (availability->total-wtus
                      availability))
  (list name
        (round-to-hundredth spare-wtus)
        (round-to-hundredth total-wtus)))

(define (semester-instructor-availability
         [name : Symbol] [schedule : InstructorSA] [this-cycle : CatalogCycle]
         [availability : Sexp])
  : (List Symbol Real Real)
  (define checky (λ ([qtrs : (Listof (U 'f 's))]
                     [wtus : Real])
                   (check-wtus this-cycle name schedule qtrs wtus)))
  (define spare-wtus
    ;; cast must succeed by earlier intersection check:
    (match availability
      ['tt-standard (checky '(f s) tt-sem-standard-wtus)]
      ['tt-first-year (checky '(f s) tt-sem-first-year-wtus)]
      ['tt-second-year (checky '(f s) tt-sem-second-year-wtus)]
      ['lec-standard (checky '(f s) lec-sem-standard-wtus)]
      ['absent (checky '(f s) absent-wtus)]
      [(list 'total (? real? wtus)) (checky '(f s) wtus)]
      [(list (list 'f (? real? fall-wtus))
             (list 's (? real? spring-wtus)))
       (+ (checky '(f) fall-wtus)
          (checky '(s) spring-wtus))]
      ;; perform no checks, return zero.
      ['not-ours 0]
      [other (error 'semester-spare-wtus "unrecognized availability format (1): ~e" other)]))
  (define total-wtus (availability->total-wtus
                      availability
                      #:sem #t))
  (list name
        (round-to-hundredth spare-wtus)
        (round-to-hundredth total-wtus)))

(define (availability->total-wtus [availability : Sexp] #:sem [semester? #f])
  ;; cast must succeed by earlier intersection check:
  (match availability
    ['tt-standard (if semester? tt-sem-standard-wtus tt-qtr-standard-wtus)]
    ['tt-first-year (if semester? tt-sem-first-year-wtus tt-qtr-first-year-wtus)]
    ['tt-second-year (if semester? tt-sem-second-year-wtus tt-qtr-second-year-wtus)]
    ['lec-standard (if semester? lec-sem-standard-wtus lec-qtr-standard-wtus)]
    ['absent absent-wtus]
    [(list 'total (? real? wtus)) wtus]
    [(list (list 'f (? real? fall-wtus))
           (list 'w (? real? winter-wtus))
           (list 's (? real? spring-wtus)))
     (unless (not semester?)
       (error 'availability->total-wtus
              "semester availability should not include winter availability"))
     (+ fall-wtus
        winter-wtus
        spring-wtus)]
    [(list (list 'f (? real? fall-wtus))
           (list 's (? real? spring-wtus)))
     (unless semester?
       (error 'availability->total-wtus
              "quarter availability should include winter availability"))
     (+ fall-wtus
        spring-wtus)]
    [(list (or 'fall-winter 'fall-spring 'winter-spring) (? real? wtus))
     (unless (not semester?)
       (error 'availability->total-wtus
              "semester availability should not be stated as ~e" availability))
     wtus]
    ;; perform no checks, return zero.
    ['not-ours 0]
    [other (error 'spare-wtus "unrecognized availability format (2): ~e" other)]))



;; ensure that the sum of the scheduled wtus are <= to the limit,
;; then return spare wtus
(define (check-wtus [this-cycle : CatalogCycle]
                    [name : Symbol] [schedule : (U InstructorSA InstructorA)]
                    [terms : (Listof (U 'f 'w 's))]
                    [limit : Real])
    (define wtu-sum
      (apply
       +
       (map (λ ([c : Assignment]) (assignment-wtus this-cycle c))
            (apply (inst append Assignment)
                   (for/list : (Listof (Listof Assignment))
                     ([t (in-list terms)])
                     ;; cast should succeed because q in '(f w s)
                     (cdr (cast (assoc t (cdr schedule))
                                (Pairof Symbol QuarterA))))))))
    (when (< (+ limit 1e-5) wtu-sum)
      (printf "instructor ~v has ~v > ~v wtus for quarters ~v\n"
              name wtu-sum limit terms))
    (- limit wtu-sum))


(define (round-to-hundredth [n : Real]) : Real
  (/ (round (* n 100)) 100))


(module+ test
  (require typed/rackunit)
  (check-equal? (availability->total-wtus 'lec-standard) 45)
  (check-equal? (availability->total-wtus 'lec-standard #:sem #t) 30)
  (check-equal? (availability->total-wtus '(fall-winter 3.3)) 3.3)
  (check-equal? (availability->total-wtus '((f 10) (s 10)) #:sem #t) 20)
  
  (check-equal? (availability->total-wtus '(total 30) #:sem #t) 30))