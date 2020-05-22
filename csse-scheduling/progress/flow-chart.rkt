#lang typed/racket

;; this file contains the information of the flow chart. Note that
;; many courses are suggested for multiple quarters, so for each course,
;; we indicate a *list* of the quarters in which it's suggested.


;; also, note that this is Cal Poly's idiosyncratic use of the term "flow
;; chart" to indicate a suggested schedule.

(require "../types.rkt"
         "../qtr-math.rkt")

(provide all-flowcharts
         flow-chart->qtr-load
         student-to-take

         Seats-By-Requirement)

;; important invariant: the strings here must correspond exactly to
;; the strings used to describe the requirements...

;; for each requirement, in which quarters are they expected to
;; take it? quarters are numbered starting at 1 to reduce the likelihood
;; of accidental error

(define-type Flowchart-Spec
  (Listof (Pairof ReqName (Listof Natural))))

(define-type Seats-By-Requirement
  (Listof (List ReqName Real)))

;; note that the ordering of the requirements within a quarter
;; is not entirely unimportant,
;; as it can affect which courses are chosen (but only on the very edge)
(define csc-core : Flowchart-Spec
  '(("csc101" 2)
    ("csc202" 3)
    ("csc203" 4)
    ("csc225" 4 5 6)
    ((csc-SE) 7)
    ("csc348" 5 6)
    ("csc357" 5 6 7)
    ((upper-level-csc-TE) 11)
    ((csc-TE-0) 8)
    ((csc-TE-1) 8)
    ((csc-TE-2) 9)
    ((csc-TE-3) 10)
    ))

(define csc-2017-2019-flowchart/pre : Flowchart-Spec
  (append
   csc-core
   '(("csc300" 7 8 9 10 11)
     ("cpe315" 5 6 7 8 9)
     ("csc349" 7)
     ("csc430" 8)
     ("csc431" 9)
     ("csc445" 10)
     ("csc453" 7 8 9 10 11 12)
     ("csc491" 11)
     ("csc492" 12)
     ((csc-TE/special-problems) 11)
     )))

(define csc-2020-2021-flowchart/pre : Flowchart-Spec
  (append
   csc-core
   '(((ethics) 7 8 9)
     ("cpe315" 5 6 7)
     ("csc349" 6 7) 
     ("csc430" 7 8)
     ((csc-TE-4) 9 10 11)
     ("csc445" 10 11)
     ("csc453" 10 11 12)
     ((csc-sp-1) 11)
     ((csc-sp-2) 12)
     ((csc-TE/special-problems) 12)
     )))

(define common-se-flowchart/pre : Flowchart-Spec
  '(("csc101" 2)
    ("csc202" 3)
    ("csc203" 4)
    ("csc225" 4)    
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
    ((upper-level-se-TE) 11)
    ((special-problems/se-TE) 10)
    ((se-TE-0) 9)
    ((se-TE-1) 10)
    ))

(define se-2017-2019-flowchart/pre : Flowchart-Spec
  (append
   common-se-flowchart/pre
   '(("csc300" 7 8 9 10 11 12)
     ((se-TE-2) 7)
     ("csc491" 11)
     ("csc492" 12))))

(define se-2019-2020-flowchart/pre : Flowchart-Spec
  (append
   common-se-flowchart/pre
   '(((ethics) 4 5 6)
    ("csc365" 7))))

(define common-cpe-flowchart/pre : Flowchart-Spec
  '(("cpe100" 1);
    ("csc101" 2);
    ("csc202" 3);
    ("csc203" 4);
    
    ("cpe233" 5);
    ("csc357" 6 7);
    ("cpe350" 10);
    ("cpe450" 11);
    ("csc453" 9 10 11 12);
    ("cpe464" 9 10 11 12);
    ("csc348" 6 7);
    ((cpe-TE/400) 12)
    ((cpe-TE-1) 11)))

(define cpe-2017-2019-flowchart/pre : Flowchart-Spec
  (append common-cpe-flowchart/pre
          '(("cpe133" 3)
            ("cpe329" 9)
            ("cpe315" 8 9)
            ("cpe461" 11)
            ("cpe462" 12)
            ((cpe-TE-0) 10))))

(define cpe-2019-2020-flowchart/pre : Flowchart-Spec
  (append common-cpe-flowchart/pre
          '(("cpe133" 4)
            ((cpe-arch) 9)
            ((microcon) 9 10)
            ((cpe-sp-1) 11)
            ((cpe-sp-2) 12)
            ((cpe-TE-0) 11))))

(define-syntax islac
  (syntax-rules ()
    [(_ str) (ann str LAC)]))

(define-type LAC (List Any CatalogCycle))

(define all-flowcharts : (Immutable-HashTable LAC Flowchart-Spec)
  (make-immutable-hash
   (ann
    ;; for FTF (they must take 123:)
    (list (cons (islac '((CSC ftf) "2017-2019"))
                (cons '((csc-TE/123) 1) csc-2017-2019-flowchart/pre))
          (cons (islac '((CSC) "2017-2019"))
                (append csc-2017-2019-flowchart/pre '(((csc-TE/123) 12))))

          (cons (islac '((CSC ftf) "2019-2020"))
                (cons '((csc-TE/123) 1) csc-2020-2021-flowchart/pre))
          (cons (islac '((CSC) "2019-2020"))
                (append csc-2020-2021-flowchart/pre '(((csc-TE/123) 12))))

          (cons (islac '((CSC ftf) "2020-2021"))
                (cons '((csc-TE/123) 1) csc-2020-2021-flowchart/pre))
          (cons (islac '((CSC) "2020-2021"))
                (append csc-2020-2021-flowchart/pre '(((csc-TE/123) 12))))

          (cons (islac '((SE ftf) "2017-2019"))
                (cons '((se-TE/123) 1) se-2017-2019-flowchart/pre))
          (cons (islac '((SE) "2017-2019"))
                (append se-2017-2019-flowchart/pre '(((se-TE/123) 12))))

          (cons (islac '((SE ftf) "2019-2020"))
                (cons '((se-TE/123) 1) se-2019-2020-flowchart/pre))
          (cons (islac '((SE) "2019-2020"))
                (append se-2019-2020-flowchart/pre '(((se-TE/123) 12))))

          (cons (islac '((CPE ftf) "2017-2019"))
                (cons '((cpe-TE/123) 1) cpe-2017-2019-flowchart/pre))
          (cons (islac '((CPE) "2017-2019"))
                (append cpe-2017-2019-flowchart/pre '(((cpe-TE/123) 12))))

          (cons (islac '((CPE ftf) "2019-2020"))
                (cons '((cpe-TE/123) 1) cpe-2019-2020-flowchart/pre))
          (cons (islac '((CPE) "2019-2020"))
                (append cpe-2019-2020-flowchart/pre '(((cpe-TE/123) 12)))))
    (Listof (Pair LAC Flowchart-Spec)))))


;; if only...
(define max-program-qtrs 12)

(when (not (andmap (λ (x) (and (integer? x)
                               (<= 1 x max-program-qtrs)))
                   (apply
                    append
                    (map (inst cdr Any (Listof Natural))
                         (apply append
                                (hash-values all-flowcharts))))))
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

(define (major->qtr-load [major : (U "CSC" "SE" "CPE")] [cc : CatalogCycle]) : Qtr-Load
  (define key (list (list (string->symbol major) 'ftf) cc))
  (define flow-chart (hash-ref all-flowcharts key))
  (flow-chart->qtr-load flow-chart))

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
                         [first-time-first-year? : Boolean]
                         [cc : CatalogCycle])
  : Seats-By-Requirement
  (define major-sym (string->symbol major))
  (define flavor
    (cond [first-time-first-year? (list major-sym 'ftf)]
          [else (list major-sym)]))
  (define flow-chart (hash-ref all-flowcharts (list flavor cc)))
  (define qtr-load (major->qtr-load major cc))
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
  (define csc-qtr-load (flow-chart->qtr-load (hash-ref all-flowcharts
                                                       '((CSC ftf) "2017-2019"))))
  (check-equal? (reqs->qtrs-done csc-qtr-load 0) 12)
  (check-equal? (reqs->qtrs-done csc-qtr-load 23) 0)
  (check-equal? (reqs->qtrs-done csc-qtr-load 9) 8)
  (check-equal? (reqs->qtrs-done csc-qtr-load 10) 7)

  
  (check-equal? (list->set
                 (filter-flow-chart (hash-ref all-flowcharts '((SE) "2017-2019"))
                                   '("csc406" "csc300")))
                (list->set
                 '(("csc300" 7 1/6)
                  ("csc300" 8 1/6)
                  ("csc300" 9 1/6)
                  ("csc300" 10 1/6)
                  ("csc300" 11 1/6)
                  ("csc300" 12 1/6)
                  ("csc406" 12 1))))

  (define example-unmet
    '("cpe315" "cpe329" "cpe350" "cpe450" "csc453"
               "cpe461" "cpe462" "cpe464" (cpe-TE/400)
               (cpe-TE/123) (cpe-TE-0) (cpe-TE-1)))
  (define cpe-qtr-load (flow-chart->qtr-load
                        (hash-ref all-flowcharts '((CPE ftf) "2017-2019"))))
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
    (student-to-take example-unmet "CPE" 0 3 #f "2017-2019"))
   (list->set
    '(("cpe315" 1)
      ("cpe329" 1)
      ("csc453" 1/4)
      ("cpe464" 1/4)
      ("cpe350" 1)
      )))

  (check-equal?
   (list->set
    (student-to-take example-unmet "CPE" 1 3 #f "2017-2019"))
   (list->set
    '(("cpe329" 1)
      ("cpe315" 1/2)
      ("cpe350" 1))))

  (check-equal?
   (list->set
    (student-to-take example-unmet "CPE" 3 3 #f "2017-2019"))
   (set))

  (check-equal? (student-to-take '("csc348") "CPE" 0 3 #f "2017-2019")
              '(("csc348" 1)))
  (check-equal? (student-to-take '("csc348") "CPE" 0 0 #f "2017-2019")
              '())
  (check-equal? (student-to-take '() "CSC" 0 3 #f "2017-2019") '()))



