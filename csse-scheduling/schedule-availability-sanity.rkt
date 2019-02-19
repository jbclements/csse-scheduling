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
;; to spare wtus
(define (spare-capacity-check [schedule : Schedule]
                              [availability : (Listof (List Symbol
                                                            Sexp))])
  : (Listof (List Symbol Real))

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



  (for/list : (Listof (List Symbol Real))
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
        ['tt-standard (checky '(f w s) 30)]
        ['lec-standard (checky '(f w s) 45)]
        ['absent (checky '(f w s) 0)]
        [(list 'total (? real? wtus)) (checky '(f w s) wtus)]
        [(list (list 'f (? real? fall-wtus))
               (list 'w (? real? winter-wtus))
               (list 's (? real? spring-wtus)))
         (+ (checky '(f) fall-wtus)
            (checky '(w) winter-wtus)
            (checky '(s) spring-wtus))]
        [(list 'fw (? real? wtus))
         (+ (checky '(f w) wtus)
            (checky '(s) 0))]
        [other (error 'zzzzz "www: ~e" other)]))
    (list name (round-to-hundredth spare-wtus))))



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



  (define (2019-course-wtus/hacked [id : Course-Id] [size : Natural])
    (match id
      ["csc570" 4]
      [other (2019-course-wtus id size)]))

  ;; # of wtus for a courseA 
  (define (courseA-wtus [this-cycle : CatalogCycle]
                        [courseA : CourseA]) : Real
    (when (not (equal? this-cycle "2019-2021"))
      (error 'courseA-wtus
             "time to generalize this function"))
    (2019-course-wtus/hacked
     (canonicalize-topic this-cycle
                         (course-topic courseA))
     (courseA-size courseA)))

  (define (round-to-hundredth [n : Real]) : Real
    (/ (round (* n 100)) 100))






