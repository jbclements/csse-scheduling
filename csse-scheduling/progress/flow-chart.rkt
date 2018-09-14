#lang racket

;; this file contains the information of the flow chart. Note that
;; many courses are suggested for multiple quarters, so for each course,
;; we indicate a *list* of the quarters in which it's suggested.

(provide csc-2017-2019-flowchart
         cpe-2017-2019-flowchart
         se-2017-2019-flowchart
         csc-qtr-load
         se-qtr-load
         cpe-qtr-load
         student-to-take)

;; there are also some interesting hacks related to, e.g., the
;; "123-or-technical-elective" course, which is just treated as
;; 123 in the flow chart.

;; important invariant: the strings here must correspond exactly to
;; the strings used to describe the requirements...

;; for each requirement, in which quarters are they expected to
;; take it? quarters are numbered starting at 1 to reduce the likelihood
;; of accidental error

;; note that the ordering of the requirements is not entirely unimportant,
;; as it can affect which courses are chosen (but only on the very edge)
(define csc-2017-2019-flowchart
  '(("csc-TE/123" 1)
    ("csc101" 2)
    ("csc202" 3)
    ("csc203" 4)
    ("csc225" 4 5 6)
    ("csc300" 7 8 9 10 11)
    ("csc-SE" 7)
    ("cpe315" 5 6 7 8 9)
    ("discrete" 5 6)
    ("csc349" 7)
    ("csc357" 5 6 7)
    ("csc430" 8)
    ("csc431" 9)
    ("csc445" 10)
    ("csc453" 7 8 9 10 11 12)
    ("csc491" 11)
    ("csc492" 12)
    ("upper-level-csc-TE" 11)
    ("csc-TE/special-problems" 11)
    ("csc-TE-0" 8)
    ("csc-TE-1" 8)
    ("csc-TE-2" 9)
    ("csc-TE-3" 10)))

(define se-2017-2019-flowchart
  '(("se-TE/123" 1)
    ("csc101" 2)
    ("csc202" 3)
    ("csc203" 4)
    ("csc225" 4)
    ("csc300" 7 8 9 10 11 12)
    ("csc305" 8)
    ("csc308" 7)
    ("csc309" 8)
    ("discrete" 5)
    ("csc349" 7)
    ("csc357" 5 6)
    ("csc402" 10)
    ("csc405" 11)
    ("csc406" 12)
    ("csc430" 9)
    ("csc484" 8)
    ("csc491" 11)
    ("csc492" 12)
    ("upper-level-se-TE" 11)
    ("special-problems/se-TE" 10)
    ("se-TE-0" 7)
    ("se-TE-1" 9)
    ("se-TE-2" 10)))

(define cpe-2017-2019-flowchart
  '(("cpe-TE/123" 1)
    ("csc101" 2)
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
    ("discrete" 6 7)
    ("cpe-TE/400" 12)
    ("cpe-TE-1" 10)
    ("cpe-TE-2" 11)))

;; if only...
(define max-program-qtrs 12)

(when (not (andmap (λ (x) (and (integer? x)
                               (<= 1 x max-program-qtrs)))
                   (apply
                    append
                    (map rest
                         (apply append
                                (list csc-2017-2019-flowchart
                                      se-2017-2019-flowchart
                                      cpe-2017-2019-flowchart))))))
  (error 'qtr-check
         "one or more flowchart entries are not in the legal range"))

;; given a flowchart, split it into a list of tuples
;; of the form requirement name x qtr x fraction,
;; sorted by qtr.
(define (flow-chart->tuple-style flowchart)
  (sort
   (apply
    append
    (for/list ([req-qtr-list (in-list flowchart)])
      (define qtr-list (rest req-qtr-list))
      (define l (length qtr-list))
      (for/list ([qtr (in-list qtr-list)])
        (list (first req-qtr-list)
              qtr
              (/ 1 l)))))
   <
   #:key second))

(define (flow-chart->qtr-load flowchart)
  (define split-flowchart (flow-chart->tuple-style flowchart))

  (define table (map (λ (grp)
                       (cons (second (first grp))
                             (apply + (map third grp))))
                     (group-by second split-flowchart)))
  (for/list ([qtr (in-range 1 13)])
    (cdr (assoc qtr table))))

(define csc-qtr-load (flow-chart->qtr-load csc-2017-2019-flowchart))
(define se-qtr-load (flow-chart->qtr-load se-2017-2019-flowchart))
(define cpe-qtr-load (flow-chart->qtr-load cpe-2017-2019-flowchart))


;; given a qtr-load and a number of unmet requirements, infer the
;; the number of quarters completed. This is definitely pretty approximate.
(define (reqs->qtrs-done qtr-load reqs-left)
  (when (> reqs-left (apply + qtr-load))
    (raise-argument-error 'reqs->qtrs-done
                          "reqs-left < sum of qtr-load"
                          1 qtr-load reqs-left))
  ;; start at the top and work down...
  (let loop ([qtrs-done 12]
             [reqs-left reqs-left]
             [qtr-loads (reverse qtr-load)])
    (cond [(<= reqs-left 0) qtrs-done]
          [(empty? qtr-loads)
           (error 'reqs->qtrs-done
                  "internal error, should be impossible by earlier check")]
          [else (loop (sub1 qtrs-done)
                      (- reqs-left (first qtr-loads))
                      (rest qtr-loads))])))

;; given a flowchart and a list of requirements remaining, return
;; a sorted list of req-qtr-load tuples
(define (filter-flow-chart flow-chart reqs-left)
  (define tuple-ish (flow-chart->tuple-style flow-chart))
  (define result
    (filter (λ (tup) (member (first tup) reqs-left)) tuple-ish))
  ;; quick sanity check...
  (define sum-of-result-reqs (apply + (map third result)))
  (unless (= (length reqs-left) sum-of-result-reqs)
    (error 'filter-flow-chart
           "expected reqs to sum to ~v, got ~v in result ~v"
           (length reqs-left)
           sum-of-result-reqs
           result))
  result)


;; given a list of requirements remaining (as a list of strings)
;; and a major, compute the set of
;; estimated requirements to be satisfied in the coming year
;; (as a list of tuples)
(define (student-to-take unmet-reqs major)
  (define flow-chart
    (match major
      ["CSC" csc-2017-2019-flowchart]
      ["SE" se-2017-2019-flowchart]
      ["CPE" cpe-2017-2019-flowchart]
      [_ (raise-argument-error
          'student-to-take
          "major string (\"CSC\",\"SE\",\"CPE\")"
          1 unmet-reqs major)]))
  (define qtr-load (flow-chart->qtr-load flow-chart))
  ;; this helps us decide how many requirements they're likely to
  ;; want to satisfy in the coming year:
  (define req-count (student-req-count unmet-reqs qtr-load))
  (define ordered-remaining-reqs
    (filter-flow-chart flow-chart unmet-reqs))
  ;; take the shortest prefix that has enough units:
  (define reqs-to-take
    (let loop ([reqs ordered-remaining-reqs]
               [total-needed req-count])
      (cond [(<= total-needed 0) '()]
            [(empty? reqs) '()]
            [else (cons (first reqs)
                        (loop (rest reqs)
                              (- total-needed
                                 (third (first reqs)))))])))
  (map (λ (grp) (list (first (first grp))
                      (apply + (map third grp))))
       (group-by first reqs-to-take)))

;; given a list of unmet requirements and a qtr-load,
;; return the number of requirements expected to be completed
;; in the next three quarters
(define (student-req-count unmet-reqs qtr-load)
  (define estimated-qtrs-completed
    (reqs->qtrs-done qtr-load (length unmet-reqs)))
  (define next-qtrs (range estimated-qtrs-completed
                           (min (+ estimated-qtrs-completed 3)
                                max-program-qtrs)))
  (define these-qtr-loads
    (for/list ([qtr (in-list next-qtrs)])
      (list-ref qtr-load qtr)))
  (apply + these-qtr-loads))







(module+ test
  (require rackunit)
  (check-equal? (reqs->qtrs-done csc-qtr-load 0) 12)
  (check-equal? (reqs->qtrs-done csc-qtr-load 23) 0)
  (check-equal? (reqs->qtrs-done csc-qtr-load (+ 9 7/15)) 8)
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
               "cpe461" "cpe462" "cpe464" "cpe-TE/400"
               "cpe-TE/123" "cpe-TE-1" "cpe-TE-2"))
  (check-equal?
   (reqs->qtrs-done cpe-qtr-load (length example-unmet))
   6)
  (check-= (student-req-count example-unmet cpe-qtr-load)
           3.5 1e-10)
  ;; in the last qtr:
  (check-= (student-req-count '("cpe461" "cpe462") cpe-qtr-load)
           2.5 1e-10)
  (check-equal?
   (list->set
    (student-to-take example-unmet "CPE"))
   (list->set
    '(("cpe-TE/123" 1)
      ("cpe315" 1)
      ("cpe329" 1)
      ("csc453" 1/4)
      ("cpe464" 1/4))))

  (check-equal? (student-to-take '("discrete") "CPE")
              '(("discrete" 1)))
  (check-equal? (student-to-take '() "CSC") '()))



