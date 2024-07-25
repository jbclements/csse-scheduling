
#lang typed/racket

;; this file checks that the instructors don't have more units
;; scheduled than they have available

(require "read-schedule.rkt"
         "scheduled-by-csse-dept.rkt"
         "types.rkt"
         "qtr-math.rkt")

(provide spare-capacity-check)

;; given a schedule and availability, provide warnings about
;; mismatches between the lists of instructors, then provide
;; warnings about instructors that are over their specified
;; availabilities, then return an association list from instructor
;; to spare wtus and total wtus
(define (spare-capacity-check [schedule : Schedule]
                              [availability : (Listof (List Symbol
                                                            Sexp))])
  : (Listof (List Symbol Real Real))

  (define this-cycle (qtr->catalog-cycle (first schedule)))
  (define scheduled (rest schedule))
  ;; names from schedule
  (define scheduled-names (map (inst first Symbol) scheduled))
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


  (define tt-standard-wtus 30)
  (define tt-first-year-wtus 20)
  (define tt-second-year-wtus 20)
  (define lec-standard-wtus 45)
  (define absent-wtus 0)

  (for/list : (Listof (List Symbol Real Real))
    ([name (in-list instructors)])
    ;; cast can't fail by earlier intersection check:
    (define schedule (cast (assoc name scheduled) InstructorA))
    (define checky (λ ([qtrs : (Listof (U 'f 'w 's))]
                       [wtus : Real])
                     (check-wtus this-cycle name schedule qtrs wtus)))
    (define spare-wtus
      ;; cast must succeed by earlier intersection check:
      (match (second (cast (assoc name availability)
                           (List Symbol Sexp)))
        ['tt-standard (checky '(f w s) tt-standard-wtus)]
        ['tt-first-year (checky '(f w s) tt-first-year-wtus)]
        ['tt-second-year (checky '(f w s) tt-second-year-wtus)]
        ['lec-standard (checky '(f w s) lec-standard-wtus)]
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
        [other (error 'spare-wtus "unrecognized availability format: ~e" other)]))
    ;; total wtus. could probably abstract over this & the previous, by making checky
    ;; an argument to a HO function...
    (define total-wtus
      ;; cast must succeed by earlier intersection check:
      (match (second (cast (assoc name availability)
                           (List Symbol Sexp)))
        ['tt-standard tt-standard-wtus]
        ['tt-first-year tt-first-year-wtus]
        ['tt-second-year tt-second-year-wtus]
        ['lec-standard lec-standard-wtus]
        ['absent absent-wtus]
        [(list 'total (? real? wtus)) wtus]
        [(list (list 'f (? real? fall-wtus))
               (list 'w (? real? winter-wtus))
               (list 's (? real? spring-wtus)))
         (+ fall-wtus
            winter-wtus
            spring-wtus)]
        [(list 'fall-winter (? real? wtus))
         wtus]
        [(list 'winter-spring (? real? wtus))
         wtus]
        ;; perform no checks, return zero.
        ['not-ours 0]
        [other (error 'spare-wtus "unrecognized availability format: ~e" other)]))
    (list name
          (round-to-hundredth spare-wtus)
          (round-to-hundredth total-wtus))))



;; ensure that the sum of the scheduled wtus are <= to the limit,
;; then return spare wtus
(define (check-wtus [this-cycle : CatalogCycle]
                    [name : Symbol] [schedule : InstructorA]
                    [qtrs : (Listof (U 'f 'w 's))]
                    [limit : Real])
    (define wtu-sum
      (apply
       +
       (map (λ ([c : CourseA]) (courseA-wtus this-cycle c))
            (apply (inst append CourseA)
                   (for/list : (Listof (Listof CourseA))
                     ([q (in-list qtrs)])
                     ;; cast should succeed because q in '(f w s)
                     (cdr (cast (assoc q (cdr schedule))
                                (Pairof Symbol QuarterA))))))))
    (when (< (+ limit 1e-5) wtu-sum)
      (printf "instructor ~v has ~v > ~v wtus for quarters ~v\n"
              name wtu-sum limit qtrs))
    (- limit wtu-sum))


(define (round-to-hundredth [n : Real]) : Real
  (/ (round (* n 100)) 100))








