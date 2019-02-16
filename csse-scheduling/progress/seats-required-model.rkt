#lang typed/racket/base

;; the goal here is to use actual student records to predict
;; how many sections of each class are required.

;; the basic idea is to take each student, and count their
;; unmet requirements to determine how many quarters from
;; graduation they are, then to use the flow chart and
;; remove the classes they've already taken to determine
;; which classes they'll need in the next year.

;; this is the "per-student" model


(require "student-progress.rkt"
         "student-progress-table.rkt"
         "degree-requirements.rkt"
         "flow-chart.rkt"
         racket/set
         racket/list
         racket/match
         "../types.rkt"
         "../canonicalize.rkt")

(provide seat-requirements/range
         seats-required
         seats-required/range)

(define (check-req-names [label : String]
                         [set1 : (Listof (Pairof ReqName Any))]
                         [set2 : (Listof (Pairof ReqName Any))])
  (define set1-names (map (inst car ReqName) set1))
  (define set2-names (map (inst car ReqName) set2))
  (unless (empty? (set-subtract set1-names set2-names))
    (error 'name-check "name-check ~a failed: first set has ~e"
           (set-subtract set1-names set2-names)))
  (unless (empty? (set-subtract set2-names set1-names))
    (error 'name-check "name-check ~a failed: second set has ~e"
           (set-subtract set2-names set1-names))))

(check-req-names "csc" csc-2017-2019-flowchart csc-requirements)
(check-req-names "cpe" cpe-2017-2019-flowchart cpe-requirements)
(check-req-names "se"  se-2017-2019-flowchart  se-requirements)

(define (first-year? [student : Student])
  (equal? (Student-entry-qtr student) 'pre-poly))

;; given a student, return the courses she'd be expected to take
;; from qtr 'start-qtr' stopping just before 'stop-qtr'
;; NOTE: In this context, qtrs are naturals such as 0 or 1, counting the number of
;; quarters from the current one.
;; returns a "Seats-By-Requirement" : (Listof (List Requirement Nonnegative-Real))
(define (student->courses [student : Student] [start-qtr : Natural] [stop-qtr : Natural])
  : Seats-By-Requirement
  (define unmet-reqs (student->unmet-requirements student))
  (student-to-take unmet-reqs (Student-major student) start-qtr stop-qtr
                   (first-year? student)))




(define (major->category [major : String])
  (match major
    ["CPE" 'cpe-bs]
    ["CSC" 'csc-bs]
    ["SE" 'se-bs]
    [other (raise-argument-error 'major->category
                                 "known major"
                                 0 major)]))

;; given a version-string and a 'start' qtr and a 'stop' qtr
;; expressed as offsets from the current qtr, return the number
;; of seats required in each class starting in the given start
;; qtr and ending one before the given stop qtr. So, for instance,
;; start 2 and stop 5 would skip the first two quarters and
;; model the next three.
(define (seat-requirements/range [version-str : String] [start-qtr : Natural] [stop-qtr : Natural]
                              [omit-first-year? : Boolean #f])
  : (Listof Seat-Requirement)
  (define students (get-students version-str))
  (define chosen-students
    (cond [omit-first-year?
           (filter (compose not first-year?) students)]
          [else students]))
  ;; tuples of (list major (listof requirement))
  (define all-to-take : (Listof (List Major-Abbr Seats-By-Requirement))
    (for/list ([i (in-naturals)]
               [student (in-list chosen-students)])
      (list (Student-major student)
            (student->courses student start-qtr stop-qtr))))
  (define all-to-take-by-major (group-by (inst first Major-Abbr) all-to-take))
  (apply
   append
   (for/list :(Listof (Listof Seat-Requirement))
     ([major-grp (in-list all-to-take-by-major)])
     (define category (major->category (first (first major-grp))))
     (define sbr (map (inst second Any Seats-By-Requirement) major-grp))
     (define by-course (group-by (inst first ReqName)
                                 (apply append sbr)))
     (for/list : (Listof Seat-Requirement)
       ([grp (in-list by-course)])
       (Seat-Requirement
        category
        (first (first grp))
        #f
        (apply + (map (inst second Any Real) grp))))
     )))

;; provide the old interface for existing users:
(define (seats-required/range [version-str : String]
                              [start-qtr : Natural]
                              [stop-qtr : Natural]
                              [omit-first-year? : Boolean #f])
  : (Listof (List ReqName Real))
  (define requirements (seat-requirements/range version-str
                                                start-qtr
                                                stop-qtr
                                                omit-first-year?))
  (define tuples : (Listof (List ReqName Real))
    (for/list ([req (in-list requirements)])
      (list (Seat-Requirement-course req)
            (Seat-Requirement-seats req))))
  ((inst sort (List ReqName Real) ReqName)
   tuples
   req-name<?
   #:key (ann first ((List ReqName Real) -> ReqName))))

;; given a version-string and a number of quarters to predict,
;; return the number of seats of each requirement required
(define (seats-required [version-str : String] [qtrs-to-predict : Natural]
                        [omit-first-year? : Boolean #f])
  : (Listof (List ReqName Real))
  (seats-required/range version-str 0 qtrs-to-predict omit-first-year?))

(define (req-name<? [a : ReqName] [b : ReqName]) : Boolean
  (cond [(string? a)
         (cond [(string? b) (string<? (course-key a)
                                      (course-key b))]
               [else #t])]
        [else
         (cond [(string? b) #f]
               [else (string<? (symbol->string (first a))
                               (symbol->string (first b)))])]))

(module+ main
  (seats-required/range "2188-1" 2 5 #f))


