#lang typed/racket/base

(provide (all-defined-out))

(require (only-in racket/list first second third fourth))

(define-type Qtr Natural)
(define-type CPTN Natural) ;; Cal Poly Term Number. Replaces Qtr, essentially
(define-type Course-Id String)
(define-type Grade String)
(define-type Units Real)
(define-type Grade-Record (List Qtr Course-Id Units Grade))

(define gr-term : (Grade-Record -> CPTN) first)
(define gr-qtr gr-term) ;; bridge
(define gr-course : (Grade-Record -> Course-Id) second)
(define gr-units : (Grade-Record -> Units) third)
(define gr-grade : (Grade-Record -> String) fourth)

;; the name of a graduation-requirement
;; ... combine with Course-Or-Group
(define-type ReqName Course-Or-Group)

(define-type Major-Abbr (U "CSC" "CPE" "SE" "EE"))

(define-type Course-Or-Group (U Course-Id CourseGroup))
(define-type CourseGroup (Listof Symbol))

(define-type Maybe-Qtr Maybe-CPTN)
(define-type Maybe-CPTN (U CPTN #f))

;; a requirement for a number of seats
(struct Seat-Requirement ([label : Category] ;; which population is this? (used for prioritizing)
                          [course : Course-Or-Group] ;; what course do they want?
                          [term-req : Maybe-CPTN] ;; when do they want it?
                          [seats : Real])  ;; how many do they want?
  #:transparent)

;; bridge, maybe unneeded
(define Seat-Requirement-qtr-req Seat-Requirement-term-req)

;; a requirement for a number of sections
(struct Section-Requirement ([label : Category] ;; which requirement is this? (used for prioritizing)
                             [course : Course-Or-Group] ;; what course do they want?
                             [term-req : Maybe-CPTN] ;; when do they want it?
                             [sections : Real])  ;; how many do they want?
  #:transparent)

;; bridge, maybe unneeded
(define Section-Requirement-qtr-req Section-Requirement-term-req)


;; an association from requirement-names to seats
(define-type Seats-By-Requirement
  (Listof (List ReqName Real)))

;; a category essentially represents a population, used to tag
;; a requirement
;; NOTE: there's definitely overlap. csc-bs and csc-bs-firstyear overlap,
;; and so do csc-bs and csse-cia-minor
(define-type Category (U 'csc-bs 'cpe-bs 'se-bs 'ee-bs 'bmed-bs
                         'enve-bs 'gene-bs 'ime-bs 'mate-bs 'me-bs
                         'csc-bs-firstyear 'cpe-bs-firstyear 'se-bs-firstyear
                         'csc-new-transfer 'cpe-new-transfer 'se-new-transfer
                         'ee-bs-firstyear
                         'ee-new-transfer
                         'csc-ms
                         
                         'laes-csc-bs

                         'stat-bs-firstyear
                         'math-bs-firstyear
                         'math-bs

                         'arce-bs
                         'brae-bs

                         'grc-bs

                         'msci-bs
                         'chem-bs

                         'econ-bs

                         'cs-minor
                         
                         ;; warning! overlaps with other groups:
                         'csse-cia-minor
                         'csse-data-minor
                         'csse-bioinf-minor

                         'non-csse-data-minor
                         'non-csse-cia-minor
                         'non-csse-bioinf-minor

                         ;; laes?
                         ;; represents a once-only load 
                         (List 'one-off Real)
                         ))

