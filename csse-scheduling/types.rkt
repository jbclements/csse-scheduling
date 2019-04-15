#lang typed/racket

(provide (all-defined-out))

(define-type Qtr Natural)
(define-type Course-Id String)
(define-type Grade String)
(define-type Units Real)
(define-type Grade-Record (List Qtr Course-Id Units Grade))

(define gr-qtr : (Grade-Record -> Qtr) first)
(define gr-course : (Grade-Record -> Course-Id) second)
(define gr-units : (Grade-Record -> Units) third)
(define gr-grade : (Grade-Record -> String) fourth)

;; the name of a graduation-requirement
;; ... combine with Course-Or-Group
(define-type ReqName Course-Or-Group)

(define-type Major-Abbr (U "CSC" "CPE" "SE"))

(define-type Course-Or-Group (U Course-Id (List Symbol)))

;; a requirement for a number of seats
(struct Seat-Requirement ([label : Category] ;; which requirement is this? (used for prioritizing)
                          [course : Course-Or-Group] ;; what course do they want?
                          [qtr-req : (U Natural #f)] ;; when do they want it?
                          [seats : Real])  ;; how many do they want?
  #:transparent)

;; a requirement for a number of sections
(struct Section-Requirement ([label : Category] ;; which requirement is this? (used for prioritizing)
                             [course : Course-Or-Group] ;; what course do they want?
                             [qtr-req : (U Natural #f)] ;; when do they want it?
                             [sections : Real])  ;; how many do they want?
  #:transparent)

;; a category essentially represents a population, used to tag
;; a requirement
;; NOTE: there's definitely overlap. csc-bs and csc-bs-firstyear overlap,
;; and so do csc-bs and csse-cia-minor
(define-type Category (U 'csc-bs 'cpe-bs 'se-bs 'ee-bs 'bmed-bs
                         'enve-bs 'gene-bs 'ime-bs 'mate-bs 'me-bs
                         'csc-bs-firstyear 'cpe-bs-firstyear 'se-bs-firstyear
                         'ee-bs-firstyear
                         'csc-ms
                         
                         'laes-csc-bs

                         'stat-bs
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

                         'non-csse-data-minor
                         'non-csse-cia-minor
                         ;; laes?
                         ;; represents a once-only load 
                         (List 'one-off Real)
                         ))