#lang typed/racket

;; this file contains the information of the flow chart. Note that
;; many courses are suggested for multiple quarters, so for each course,
;; we indicate a *list* of the quarters in which it's suggested.

(require "../types.rkt")

(provide csc-2017-2019-flowchart
         cpe-2017-2019-flowchart
         se-2017-2019-flowchart
         csc-qtr-load
         se-qtr-load
         cpe-qtr-load
         student-to-take

         Seats-By-Requirement)

;; important invariant: the strings here must correspond exactly to
;; the strings used to describe the requirements...

;; for each requirement, in which quarters are they expected to
;; take it? quarters are numbered starting at 1 to reduce the likelihood
;; of accidental error

(define-type Flowchart-Spec
  (Listof (Pairof ReqName
                  (Listof Natural))))

(define-type Seats-By-Requirement
  (Listof (List ReqName Real)))

;; note that the ordering of the requirements within a quarter
;; is not entirely unimportant,
;; as it can affect which courses are chosen (but only on the very edge)
(define csc-2017-2019-flowchart/pre : Flowchart-Spec
  '(("csc101" 2)
    ("csc202" 3)
    ("csc203" 4)
    ("csc225" 4 5 6)
    ("csc300" 7 8 9 10 11)
    ((csc-SE) 7)
    ("cpe315" 5 6 7 8 9)
    ("csc348" 5 6)
    ("csc349" 7)
    ("csc357" 5 6 7)
    ("csc430" 8)
    ("csc431" 9)
    ;; UGGHHH... backing this out for now:
    ;; putting this in place of 431:
    ;;((csc-TE-4) 9)
    ("csc445" 10)
    ("csc453" 7 8 9 10 11 12)
    ("csc491" 11)
    ("csc492" 12)
    ((upper-level-csc-TE) 11)
    ((csc-TE/special-problems) 11)
    ((csc-TE-0) 8)
    ((csc-TE-1) 8)
    ((csc-TE-2) 9)
    ((csc-TE-3) 10)))

;; use this one for first-time-first-years
(define csc-2017-2019-flowchart-FTF : Flowchart-Spec
  (cons '((csc-TE/123) 1) csc-2017-2019-flowchart/pre))
;; use this one for everyone else:
(define csc-2017-2019-flowchart : Flowchart-Spec
  (cons '((csc-TE/123) 12) csc-2017-2019-flowchart/pre))

(define se-2017-2019-flowchart/pre : Flowchart-Spec
  '(("csc101" 2)
    ("csc202" 3)
    ("csc203" 4)
    ("csc225" 4)
    ("csc300" 7 8 9 10 11 12)
    ("csc305" 8)
    ("csc308" 7)
    ("csc309" 8)
    ("csc348" 5)
    ("csc349" 7)
    ("csc357" 5 6)
    ("csc402" 10)
    ("csc405" 11)
    ("csc406" 12)
    ("csc430" 9)
    ("csc484" 8)
    ("csc491" 11)
    ("csc492" 12)
    ((upper-level-se-TE) 11)
    ((special-problems/se-TE) 10)
    ((se-TE-0) 7)
    ((se-TE-1) 9)
    ((se-TE-2) 10)))

;; use this one for first-time-first-years
(define se-2017-2019-flowchart-FTF : Flowchart-Spec
  (cons '((se-TE/123) 1) se-2017-2019-flowchart/pre))
;; use this one for everyone else:
(define se-2017-2019-flowchart : Flowchart-Spec
  (cons '((se-TE/123) 12) se-2017-2019-flowchart/pre))

(define cpe-2017-2019-flowchart/pre : Flowchart-Spec
  '(("csc101" 2)
    ("csc202" 3)
    ("csc203" 4)
    ("cpe133" 3)
    ("cpe233" 5)
    ("cpe315" 8 9)
    ("cpe329" 9)
    ("csc357" 6 7)
    ("cpe350" 10)
    ("cpe450" 11)
    ("csc453" 9 10 11 12)
    ("cpe461" 11)
    ("cpe462" 12)
    ("cpe464" 9 10 11 12)
    ("csc348" 6 7)
    ((cpe-TE/400) 12)
    ((cpe-TE-1) 10)
    ((cpe-TE-2) 11)))

;; use this one for first-time-first-years
(define cpe-2017-2019-flowchart-FTF : Flowchart-Spec
  (cons '((cpe-TE/123) 1) cpe-2017-2019-flowchart/pre))
;; use this one for everyone else:
(define cpe-2017-2019-flowchart : Flowchart-Spec
  (cons '((cpe-TE/123) 12) cpe-2017-2019-flowchart/pre))

;; if only...
(define max-program-qtrs 12)

(when (not (andmap (λ (x) (and (integer? x)
                               (<= 1 x max-program-qtrs)))
                   (apply
                    append
                    (map (inst cdr Any (Listof Natural))
                         (apply append
                                (list csc-2017-2019-flowchart
                                      se-2017-2019-flowchart
                                      cpe-2017-2019-flowchart
                                      csc-2017-2019-flowchart-FTF
                                      se-2017-2019-flowchart-FTF
                                      cpe-2017-2019-flowchart-FTF))))))
  (error 'qtr-check
         "one or more flowchart entries are not in the legal range"))

;; given a flowchart, split it into a list of tuples
;; of the form requirement name x qtr x fraction,
;; sorted by qtr.
(define-type Flowchart-Tup (List ReqName Natural Nonnegative-Real))
(define tup-seats : (-> Flowchart-Tup Nonnegative-Real) third)

(define (flow-chart->tuple-style [flowchart : Flowchart-Spec])
  : (Listof Flowchart-Tup)
  ((inst sort Flowchart-Tup Natural)
   (apply
    append
    (for/list : (Listof (Listof Flowchart-Tup))
        ([req-qtr-list (in-list flowchart)])
      (define qtr-list (rest req-qtr-list))
      (define l (length qtr-list))
      (for/list : (Listof (List ReqName Natural Nonnegative-Real))
        ([qtr (in-list qtr-list)])
        (list (first req-qtr-list)
              qtr
              (/ 1 l)))))
   <
   #:key second))

;; represents the number of requirements satisfied in each quarter,
;; according to the flow chart
(define-type Qtr-Load (Listof Nonnegative-Real))
;; this should only be used with the FTF specs....
(define (flow-chart->qtr-load [flowchart : Flowchart-Spec])
  : Qtr-Load
  (define split-flowchart (flow-chart->tuple-style flowchart))

  (define table
    : (Listof (Pair Natural Nonnegative-Real))
    (map (λ ([grp : (Listof Flowchart-Tup)])
                       (cons (second (first grp))
                             (apply + (map tup-seats grp))))
                     (group-by (inst second Any Natural) split-flowchart)))
  (for/list : Qtr-Load
    ([qtr (in-range 1 13)])
    (cdr (or (assoc qtr table) (cons #f 0)))))

(define csc-qtr-load (flow-chart->qtr-load csc-2017-2019-flowchart-FTF))
(define se-qtr-load (flow-chart->qtr-load se-2017-2019-flowchart-FTF))
(define cpe-qtr-load (flow-chart->qtr-load cpe-2017-2019-flowchart-FTF))

(define (major->qtr-load [major : (U "CSC" "SE" "CPE")]) : Qtr-Load
  (match major
    ["CSC" csc-qtr-load]
    ["SE" se-qtr-load]
    ["CPE" cpe-qtr-load]))

;; given a qtr-load and a number of unmet requirements, infer the
;; the number of quarters completed. This is definitely pretty approximate.
(define (reqs->qtrs-done [qtr-load : Qtr-Load] [reqs-left : Natural])
  : Natural
  (when (> reqs-left (apply + qtr-load))
    (raise-argument-error 'reqs->qtrs-done
                          "reqs-left < sum of qtr-load"
                          1 qtr-load reqs-left))
  ;; start at the top and work down...
  (let loop ([qtrs-done : Natural 12]
             [reqs-left : Real reqs-left]
             [qtr-loads (reverse qtr-load)])
    (cond [(<= reqs-left 0) qtrs-done]
          [(empty? qtr-loads)
           (error 'reqs->qtrs-done
                  "internal error, should be impossible by earlier check")]
          [(= qtrs-done 0)
           (error 'reqs->qtrs-done
                  "internal error, should be impossible")]
          [else
           (define new-reqs-left (- reqs-left (first qtr-loads)))
           (loop (sub1 qtrs-done)
                 new-reqs-left
                 (rest qtr-loads))])))

;; given a flowchart and a list of requirements remaining, return
;; a sorted list of req-qtr-load tuples
(define (filter-flow-chart [flow-chart : Flowchart-Spec]
                           [reqs-left : (Listof ReqName)])
  : (Listof Flowchart-Tup)
  (define tuple-ish (flow-chart->tuple-style flow-chart))
  (define result
    (filter (λ ([tup : Flowchart-Tup])
              (member (first tup) reqs-left))
            tuple-ish))
  ;; quick sanity check...
  (define sum-of-result-reqs (apply + (map tup-seats result)))
  (unless (= (length reqs-left) sum-of-result-reqs)
    (let ([ans reqs-left])
      (printf "reqs-left: ~s\n" ans)
      ans)
    (error 'filter-flow-chart
           "expected reqs to sum to ~v, got ~v in result ~v"
           (length reqs-left)
           sum-of-result-reqs
           result))
  result)

;; given a list of requirements remaining (as a list of strings)
;; and a major and a number of quarters, compute the set of
;; estimated requirements to be satisfied in the coming 'qtr-count'
;; quarters (as a list of tuples)
(define (student-to-take [unmet-reqs : (Listof ReqName)]
                         [major : Major-Abbr]
                         [start-qtr : Natural]
                         [stop-qtr : Natural]
                         [first-time-first-year? : Boolean])
  : Seats-By-Requirement
  (define flow-chart
    (match major
      ["CSC" (if first-time-first-year?
                 csc-2017-2019-flowchart-FTF
                 csc-2017-2019-flowchart)]
      ["SE" (if first-time-first-year?
                 se-2017-2019-flowchart-FTF
                 se-2017-2019-flowchart)]
      ["CPE" (if first-time-first-year?
                 cpe-2017-2019-flowchart-FTF
                 cpe-2017-2019-flowchart)]
      [_ (raise-argument-error
          'student-to-take
          "major string (\"CSC\",\"SE\",\"CPE\")"
          1 unmet-reqs major)]))
  (define qtr-load (major->qtr-load major))
  (define (helper [qtr-count : Natural]) : Seats-By-Requirement
    ;; this helps us decide how many requirements they're likely to
    ;; want to satisfy in the specified period
    (define req-count (student-req-count unmet-reqs qtr-load qtr-count))
    (define ordered-remaining-reqs
      (filter-flow-chart flow-chart unmet-reqs))
    ;; take the shortest prefix that has enough units:
    (define reqs-to-take
      (let loop : (Listof Flowchart-Tup)
        ([reqs : (Listof Flowchart-Tup) ordered-remaining-reqs]
         [total-needed : Real req-count])
        (cond [(<= total-needed 0) '()]
              [(empty? reqs) '()]
              [else (cons (first reqs)
                          (loop (rest reqs)
                                (- total-needed
                                   (tup-seats (first reqs)))))])))
    (map (λ ([grp : (Listof Flowchart-Tup)])
           : (List ReqName Nonnegative-Real)
           (list (first (first grp))
                 (apply + (map tup-seats grp))))
         (group-by (inst first ReqName) reqs-to-take)))
  (define by-end-requirements (helper stop-qtr))
  (define before-start-requirements (helper start-qtr))
  (seats-subtract by-end-requirements before-start-requirements))

(require/typed racket/dict
               [dict-ref (-> Seats-By-Requirement
                             ReqName
                             (List Nonnegative-Real)
                             (List Nonnegative-Real))])

;; given two sets of requirement-seats, subtract the second from the first.
;; signal an error if a requirement would have a negative number of seats
(define (seats-subtract [req-set-a : Seats-By-Requirement]
                        [req-set-b : Seats-By-Requirement])
  : Seats-By-Requirement
  (define all-requirements
    (remove-duplicates
     (append (map (inst first ReqName) req-set-a)
             (map (inst first ReqName) req-set-b))))
  (define sbr : Seats-By-Requirement
    (for/list ([r (in-list all-requirements)])
      (define seats
        (- (first (dict-ref req-set-a r '(0)))
           (first (dict-ref req-set-b r '(0)))))
      (when (< seats 0)
        (error 'seats-subtract "negative seats required for requirement: ~v" r))
      (list r seats)))
  ;; filter out the ones with zero seats
  (filter (λ ([tup : (List ReqName Real)])
            (not (= 0 (second tup))))
          sbr))


;; given a list of unmet requirements and a qtr-load and a number
;; of quarters 'n',
;; return the number of requirements expected to be completed
;; in the next 'n' quarters
(define (student-req-count [unmet-reqs : (Listof ReqName)]
                           [qtr-load : Qtr-Load]
                           [qtrs : Natural]) : Nonnegative-Real
  (define estimated-qtrs-completed
    (reqs->qtrs-done qtr-load (length unmet-reqs)))
  (define next-qtrs (range estimated-qtrs-completed
                           (min (+ estimated-qtrs-completed
                                   qtrs)
                                max-program-qtrs)))
  (define these-qtr-loads
    (for/list : (Listof Nonnegative-Real)
      ([qtr (in-list next-qtrs)])
      (list-ref qtr-load qtr)))
  (apply + these-qtr-loads))







(module+ test
  (require typed/rackunit)
  (check-equal? (reqs->qtrs-done csc-qtr-load 0) 12)
  (check-equal? (reqs->qtrs-done csc-qtr-load 23) 0)
  (check-equal? (reqs->qtrs-done csc-qtr-load 9) 8)
  (check-equal? (reqs->qtrs-done csc-qtr-load 10) 7)

  (check-equal? (filter-flow-chart se-2017-2019-flowchart
                                   '("csc406" "csc300"))
                '(("csc300" 7 1/6)
                  ("csc300" 8 1/6)
                  ("csc300" 9 1/6)
                  ("csc300" 10 1/6)
                  ("csc300" 11 1/6)
                  ("csc300" 12 1/6)
                  ("csc406" 12 1)))

  (define example-unmet
    '("cpe315" "cpe329" "cpe350" "cpe450" "csc453"
               "cpe461" "cpe462" "cpe464" (cpe-TE/400)
               (cpe-TE/123) (cpe-TE-1) (cpe-TE-2)))
  (check-equal?
   (reqs->qtrs-done cpe-qtr-load (length example-unmet))
   6)
  (check-= (student-req-count example-unmet cpe-qtr-load 3)
           3.5 1e-10)
  (check-= (student-req-count example-unmet cpe-qtr-load 2)
           1.5 1e-10)
  (check-= (student-req-count example-unmet cpe-qtr-load 1)
           1.0 1e-10)
  ;; in the last qtr:
  (check-= (student-req-count '("cpe461" "cpe462") cpe-qtr-load 3)
           2.5 1e-10)
  (check-equal?
   (list->set
    (student-to-take example-unmet "CPE" 0 3 #f))
   (list->set
    '(("cpe315" 1)
      ("cpe329" 1)
      ("csc453" 1/4)
      ("cpe464" 1/4)
      ("cpe350" 1)
      )))

  (check-equal?
   (list->set
    (student-to-take example-unmet "CPE" 1 3 #f))
   (list->set
    '(("cpe329" 1)
      ("csc453" 1/4)
      ("cpe464" 1/4)
      ("cpe350" 1)
      )))

  (check-equal?
   (list->set
    (student-to-take example-unmet "CPE" 3 3 #f))
   (set))

  (check-equal? (student-to-take '("csc348") "CPE" 0 3 #f)
              '(("csc348" 1)))
  (check-equal? (student-to-take '("csc348") "CPE" 0 0 #f)
              '())
  (check-equal? (student-to-take '() "CSC" 0 3 #f) '()))



