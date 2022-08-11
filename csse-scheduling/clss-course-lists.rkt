#lang racket

;; this short file parses the CSV files exported by CLSS, and should accurately
;; indicate which courses are to be staffed and scheduled by each of the three
;; departments (csse, cpe, ee) that I download spreadsheets for.

;; ugh. Even with "show courses with no sections", the exported CSV doesn't include
;; these courses.

;; this code is not currently used e.g. in check-schedule.rkt etc., but it probably
;; should be. (2022-07-21)

;; to update:
;; - go to clss
;; - double-click on the department
;; - choose export
;; - check the "show courses with no sections" box
;; - include the columns "Term Code" and "Course"
;; - move the files ... well, here, for the moment.
;; - adjust (or generalize) the course

(require shelly/csv-reading-2
         racket/runtime-path
         "canonicalize.rkt"
         "qtr-math.rkt")

(define-runtime-path here ".")

(define column-names
  '("Term Code" "Course"))

(define qtr 2228)

(define col-ref (make-col-ref column-names))

(define (department-courses prefix)
  (define rows
    (remove-duplicates
     (file->rows/filter (build-path here (format "~a-clss-courses-2228.csv" prefix))
                        column-names)))

  (unless (equal? (list (~a qtr))
                  (remove-duplicates (map (λ (r) (col-ref r "Term Code")) rows)))
    (error 'bbb "moreinfo"))

  (define courses (map (λ (r) (col-ref r "Course")) rows))

  (for/list ([c (in-list courses)])
    (match c
      [(regexp #px"^(CPE|CSC|DATA|EE) ([0-9]{3})$" (list _ prefix num))
       (canonicalize (qtr->catalog-cycle qtr) prefix num)])))

;(department-courses "cpe")
(department-courses "csc")
;(department-courses "ee")

