#lang typed/racket/base

;; the goal here is to use actual student records to predict
;; how many sections of each class are required.

;; the basic idea is to take each student, and count their
;; unmet requirements to determine how many quarters from
;; graduation they are, then to use the flow chart and
;; remove the classes they've already taken to determine
;; which classes they'll need in the next year.

;; this is the "per-student" model


(require racket/set
         racket/list
         racket/match
         "student-progress.rkt"
         "student-progress-table.rkt"
         "degree-requirements.rkt"
         "flow-chart.rkt"
         "../qtr-math.rkt"
         "../types.rkt"
         "../canonicalize.rkt")

(provide seat-requirements/range
         seat-requirements-reduce
         ;seats-required
         ;seats-required/range
         student->courses)



;; these courses should be associated with specific quarters,
;; to ensure that students don't fall behind.
(define constrained-courses
  '("csc101" "csc202" "csc203" "csc225" "csc357"
             ;; experimenting?
             "cpe133" "ee211" "ee241"))

;; given a major (e.g. "csc", and two lists of pairs of ReqNames + any
;; (e.g. a list of requirements), ensure that each set has exactly the
;; same set of ReqNames
(define (check-req-names [label : Any]
                         [flowchart-set : (Listof (Pairof ReqName Any))]
                         [degree-requirement-set : (Listof (Pairof ReqName Any))])
  (define set1-names (map (inst car ReqName) flowchart-set))
  (define set2-names (map (inst car ReqName) degree-requirement-set))
  (unless (empty? (set-subtract set1-names set2-names))
    (error 'name-check "name-check for ~a failed: flowchart set has ~e"
           label
           (set-subtract set1-names set2-names)))
  (unless (empty? (set-subtract set2-names set1-names))
    (error 'name-check "name-check for ~a failed: degree requirement set has ~e"
           label
           (set-subtract set2-names set1-names)))
  (unless (equal? (list->set set1-names) (list->set set2-names))
    (error 'name-check "something else failed... duplicated req name?")))

;; check that the list of degree requirements has the same set
;; of requirements as the flow chart
(define requirements-keys (hash-keys program-requirements))
(for ([key (in-list requirements-keys)])
  (check-req-names key (hash-ref all-flowcharts key
                                 (λ ()
                                   (error 'requirements-keys "no flowchart found for key: ~e"
                                          key)))
                   (hash-ref program-requirements key)))

;; check that every group name appears in the table of requirements
(let ()
  (define requirement-names
    (map (inst first ReqName)
         (remove-duplicates (apply append (hash-values program-requirements)))))
  (define group-requirement-names
    (filter list? requirement-names))
  (define all-defined (list->set group-requirement-names))
  (define all-in-table (list->set
                        (map (λ ([n : Symbol]) : (List Symbol) (list n))
                             course-group-names)))
  ;; there are some requirements (e.g. CSCMSTHESIS) that don't appear
  ;; in the flowchart
  (unless (subset? all-defined all-in-table)
    (error 'requirement-table-check
           "sets not equal. used: ~v\nlisted in table: ~v\n"
           all-defined all-in-table)))


(define (first-year? [student : Student])
  (equal? (Student-entry-qtr student) 'pre-poly))

;; given a student, return the courses she'd be expected to take
;; from qtr 'start-qtr' stopping just before 'stop-qtr'
;; NOTE: In this context, qtrs are naturals such as 0 or 1, counting the number of
;; quarters from the current one.
;; returns a "Seats-By-Requirement" : (Listof (List Requirement Nonnegative-Real))
(define (student->courses [student : Student]
                          [start-qtr-idx : Natural]
                          [stop-qtr-idx : Natural]
                          [cc : CatalogCycle])
  : (Listof Seats-By-Requirement)
  (when (< 100 start-qtr-idx)
    (raise-argument-error 'student->courses
                          "reasonable number of quarters"
                          1 student start-qtr-idx stop-qtr-idx cc))
  (when (< 100 stop-qtr-idx)
    (raise-argument-error 'student->courses
                          "reasonable number of quarters"
                          2 student start-qtr-idx stop-qtr-idx cc))
  (define unmet-reqs (student->unmet-requirements student cc))
  (student-to-take unmet-reqs (Student-major student)
                   start-qtr-idx
                   stop-qtr-idx
                   (first-year? student)
                   cc))



;; given a major, return the standard "student category" for that student,
;; used in the generated seat-requirements
(define (major->category [major : Major-Abbr])
  (match major
    ["CPE" 'cpe-bs]
    ["CSC" 'csc-bs]
    ["SE" 'se-bs]
    ["EE" 'ee-bs]
    [other (raise-argument-error 'major->category
                                 "known major"
                                 0 major)]))


(: transpose (All (A) ((Listof (Listof A)) -> (Listof (Listof A)))))
(define (transpose lol)
  (cond [(empty? (car lol))
         '()]
        [else
         (cons (map (inst car A) lol)
               (transpose (map (inst cdr A) lol)))]))

;; given a model qtr (e.g. 2202, indicating data obtained after winter 2020),
;; and a 'start' qtr and a 'stop' qtr, return a list of Seat-Requirement's
;; indicating the requirements for each modeled quarter, starting in the given start
;; qtr and ending one before the given stop qtr. So, for instance,
;; start 2208 and stop 2214 would model two quarters, fall 2020 and
;; winter 2021.

;; some courses are highly depended-on, like 202, 203, and 357.
;; when students need these courses in a particular quarter of modeling,
;; these requirements should be tagged with the appropriate quarter,
;; so that we can see that they don't just need them any old time,
;; they need them in the appropriate quarter.
(define (seat-requirements/range [model-qtr : Qtr]
                                 [start-qtr : Qtr]
                                 [stop-qtr : Qtr]
                                 [scc : (Student -> CatalogCycle)]
                                 [omit-first-year? : Boolean])
  : (Listof (Listof Seat-Requirement))
  (define version-str (string-append (number->string model-qtr) "-1"))
  ;; cast could fail...
  (define start-idx (cast (sub1 (qtr-subtract/no-summer start-qtr (summer->spring model-qtr))) Natural))
  (define stop-idx (cast (sub1 (qtr-subtract/no-summer stop-qtr (summer->spring model-qtr))) Natural))
  (define students (get-students version-str))
  (define chosen-students
    (cond [omit-first-year?
           (filter (compose not first-year?) students)]
          [else students]))
  ;; tuples of (list major (listof (listof requirement))) (one for each student).
  (define all-to-take : (Listof (Listof (Listof (List Major-Abbr ReqName Real))))
    (for/list ([student (in-list chosen-students)])
      (define qtrs-plan (student->courses student start-idx
                                          stop-idx (scc student)))
      (for/list : (Listof (Listof (List Major-Abbr ReqName Real)))
        ([qtr-plan (in-list qtrs-plan)])
        (for/list : (Listof (List Major-Abbr ReqName Real))
          ([sr : (List ReqName Real) (in-list qtr-plan)])
          (ann (cons (Student-major student) sr)
               (List Major-Abbr ReqName Real))))))
  ;; transpose to make them by-qtr
  (define qtrs-requirements
    (transpose all-to-take))
  (for/list ([qtr-requirement (in-list qtrs-requirements)]
             [qtr : Natural (in-list (qtrs-in-range start-qtr stop-qtr))])
    (seat-requirements-reduce
     (for/list : (Listof Seat-Requirement)
       ([req (in-list (apply append qtr-requirement))])
       (Seat-Requirement
        (major->category (first req))
        (second req)
        (cond [(member (second req) constrained-courses) qtr]
              [else #f])
        (third req))))))

;; map summer to the previous spring, leave others alone.
;; this is our hack to deal with models based on summer
;; quarter data
(define (summer->spring [qtr : Natural]) : Natural
  qtr
  (cond [(equal? (qtr->season qtr) "Summer")
         (prev-qtr qtr)]
        [else qtr]))

(module+ test
  (check-equal? (summer->spring 2204) 2204)
  (check-equal? (summer->spring 2206) 2204))

;; given a list of seat requirements, combine all of those that differ
;; only in the seat count.
(define (seat-requirements-reduce [losr : (Listof Seat-Requirement)])
  : (Listof Seat-Requirement)
  (define grouped : (Listof (Listof Seat-Requirement))
    (group-by (λ ([sr : Seat-Requirement])
                (list (Seat-Requirement-label sr)
                      (Seat-Requirement-course sr)
                      (Seat-Requirement-qtr-req sr)))
              losr))
  (for/list : (Listof Seat-Requirement)
    ([g : (Listof Seat-Requirement) (in-list grouped)])
    (define f (first g))
    (Seat-Requirement
     (Seat-Requirement-label f)
     (Seat-Requirement-course f)
     (Seat-Requirement-qtr-req f)
     (apply + (map Seat-Requirement-seats g)))))

;; provide the old interface for existing users:
;; can I just dump this?
#;(define (seats-required/range [version-str : String]
                              [start-qtr : Natural]
                              [stop-qtr : Natural]
                              [cc : CatalogCycle]
                              [omit-first-year? : Boolean #f])
  : (Listof (List ReqName Real))
  (define requirements (seat-requirements/range version-str
                                                start-qtr
                                                stop-qtr
                                                cc
                                                omit-first-year?))
  (define tuples : (Listof (List ReqName Real))
    (for/list ([req (in-list requirements)])
      (list (Seat-Requirement-course req)
            (Seat-Requirement-seats req))))
  (define summed-tuples
    (for/list : (Listof (List ReqName Real))
      ([tuple-group (in-list (group-by (inst first ReqName) tuples))])
      (list (first (first tuple-group)) (apply + (map (inst second Any Real) tuple-group)))))
  ((inst sort (List ReqName Real) ReqName)
   summed-tuples
   req-name<?
   #:key (ann first ((List ReqName Real) -> ReqName))))

;; given a version-string and a number of quarters to predict,
;; return the number of seats of each requirement required
#;(define (seats-required [version-str : String] [qtrs-to-predict : Natural]
                        [cc : CatalogCycle]
                        [omit-first-year? : Boolean])
  : (Listof (List ReqName Real))
  (seats-required/range version-str 0 qtrs-to-predict cc omit-first-year?))

(define (req-name<? [a : ReqName] [b : ReqName]) : Boolean
  (cond [(string? a)
         (cond [(string? b) (string<? (course-key a)
                                      (course-key b))]
               [else #t])]
        [else
         (cond [(string? b) #f]
               [else (string<? (symbol->string (first a))
                               (symbol->string (first b)))])]))

(module+ test
  (require typed/rackunit)

  (check-equal?
   (transpose '((a b c) (d e f)))
   '((a d) (b e) (c f)))
  (check-equal?
   (seat-requirements-reduce
    (list (Seat-Requirement 'csc-bs "csc123" #f 3)
          (Seat-Requirement 'cpe-bs "csc123" #f 4)
          (Seat-Requirement 'csc-bs "cpe464" #f 5)
          (Seat-Requirement 'csc-bs "csc123" #f 6)
          (Seat-Requirement 'csc-bs "cpe464" #f 10)))
   (list (Seat-Requirement 'csc-bs "csc123" #f 9)
         (Seat-Requirement 'cpe-bs "csc123" #f 4)
         (Seat-Requirement 'csc-bs "cpe464" #f 15))))

(module+ main
  #;(filter
   (λ ([sr : Seat-Requirement])
     (member (Seat-Requirement-course sr)
             '("csc308")))
   (apply
    append
    (seat-requirements/range 2202 2204 2214 "2019-2020" #t)))

  #;(filter
   (λ ([sr : Seat-Requirement])
     (member (Seat-Requirement-course sr)
             '("csc402")))
   (apply
    append
    (seat-requirements/range 2202 2204 2214 "2019-2020" #t)))

  #;(filter
   (λ ([sr : Seat-Requirement])
     (member (Seat-Requirement-course sr)
             '("csc305")))
   #;(apply
    append
    (seat-requirements/range 2202 2204 2218 "2019-2020" #t)))

  (filter
   (λ ([sr : Seat-Requirement])
     (member (Seat-Requirement-course sr)
             '("cpe350")))
   (apply
    append
    (seat-requirements/range 2204 2208 2218 (λ (x) "2019-2020") #f)))

  

  )


