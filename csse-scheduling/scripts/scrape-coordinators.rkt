#lang racket

(require sugar
         csv-writing
         racket/runtime-path)

(define-runtime-path here ".")

(define current-catalog "2017-2019")

(define scraped-lines
  (map
   string-trim
  (filter (λ (str) (not (equal? str "")))
          (file->lines (build-path here "coordinators.tsv")))))

(define split-lines
  (map (λ (line) (string-split line "\t")) scraped-lines))

#;(define split-lines
  (slicef-at scraped-lines
             (λ (str)
               (regexp-match #px"^(CSC|CPE|CSC/CPE|CSC/ART|CS) [0-9]{3}$"
                             str))))

;; drop the first row if it's just column headers (generalize
;; this if necessary)
(define column-row-drop
  (match (first split-lines)
    [(list "Course number" _ _) (rest split-lines)]
    [_ split-lines]))

(define cleaned-lines
  (for/list ([l (in-list column-row-drop)])
    (map
     string-trim
    (match l
      [(list course description coordinator) l]
      [(list course description) (append l (list ""))]
      [_ (error 'splitting "expected group of size 2 or 3, got: ~e"
             l)]))
    ))

(call-with-output-file "/tmp/coordinators.csv"
  (λ (port) (display-table cleaned-lines port))
  #:exists 'truncate)
