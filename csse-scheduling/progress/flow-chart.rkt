#lang racket

;; this file contains the information of the flow chart. Note that
;; many courses are suggested for multiple quarters, so for each course,
;; we indicate a *list* of the quarters in which it's suggested.

(provide csc-2017-2019-flowchart
         cpe-2017-2019-flowchart
         se-2017-2019-flowchart
         csc-qtr-load
         se-qtr-load
         cpe-qtr-load)

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
    ("upper-level-tech-elect" 11)
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
  (define split-flowchart (flow-chart->tuple-style ))

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
  ;; start at the top and work down...
  (let loop ([qtrs-done 12]
             [reqs-left reqs-left]
             [qtr-loads (reverse qtr-load)])
    (cond [(<= reqs-left 0) qtrs-done]
          [else (loop (sub1 qtrs-done)
                      (- reqs-left (first qtr-loads))
                      (rest qtr-loads))])))

;; given a flowchart and a list of requirements remaining, return
;; a sorted list of req-qtr-load tuples
(define (filter-flow-chart flow-chart reqs-left)
  )


(module+ test
  (require rackunit)
  (check-equal? (reqs->qtrs-done csc-qtr-load 0) 12)
  (check-equal? (reqs->qtrs-done csc-qtr-load 23) 0)
  (check-equal? (reqs->qtrs-done csc-qtr-load (+ 9 7/15)) 8)
  (check-equal? (reqs->qtrs-done csc-qtr-load 10) 7))



