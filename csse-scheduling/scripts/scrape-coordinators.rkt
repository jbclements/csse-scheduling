#lang racket

(require sugar
         "../canonicalize.rkt")

(define current-catalog "2017-2019")

(define scraped-lines
  (map
   string-trim
  (filter (λ (str) (not (equal? str "")))
          (file->lines "/tmp/gg.txt"))))

(define split-lines
  (slicef-at scraped-lines
             (λ (str)
               (regexp-match #px"^(CSC|CPE|CSC/CPE|CSC/ART|CS) [0-9]{3}$"
                             str))))

(for ([l (in-list split-lines)])
  (unless (or (= (length l) 3)
              (= (length l) 2))
    (error 'splitting "expected group of size 2 or 3, got: ~e"
           l)))

(define coordinator-table
  (for/list ([l (in-list split-lines)])
    (define canonical-name
      (match (first l)
        ;; 2 ad-hoc fixups:
        ["CSC 419" (canonicalize current-catalog "CPE" "419")]
        ["CS 448" (canonicalize current-catalog "CSC" "448")]
        [(regexp #px"^([A-Z]+)(/[A-Z]+)? ([0-9]{3})$" (list _ subj _ num))
         (canonicalize current-catalog subj num)]))
    (define coordinator
      (match l
        [(list _ _ name)
         (regexp-split #px"/" name)]
        [(list _ _) #f]))
    (list canonical-name coordinator)))

(call-with-output-file "/tmp/coordinators.rktd"
  (λ (port)
    (pretty-write coordinator-table port)))