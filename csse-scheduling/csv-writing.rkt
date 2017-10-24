#lang racket

(provide csv-display
         csv->string)


;; given a list of lists, write in CSV style to the given port
(define (csv-display lol [maybe-port #f])
  (define port (or maybe-port (current-output-port)))
  (for ([l (in-list lol)])
    (display
     (apply
      string-append
      (add-between
       (map csv-quote l)
       ","))
     port)
    (newline port)))

;; given a list of lists, produce the corresponding CSV string
(define (csv->string lol)
  (apply
   string-append
   (for/list ([row (in-list lol)])
     (string-append (csv-row->str row) "\n"))))

;; wrap quotes around, freak out if there are any weird chars
(define (string-quote str)
  (match str
    [(regexp #px"\n\t\"") (error 'string-quote
                                 "expected clean string, got: ~e"
                                 str)]
    [_ (string-append "\"" str "\"")]))

;; prepare a number, string, or boolean for printing as a CSV value
;; ... if there's a canonical specification of how to do this, I
;; don't have it.
(define (csv-quote val)
  (cond [(boolean? val)
         (if val "TRUE" "FALSE")]
        [(number? val) (~a val)]
        [(string? val) (string-quote val)]
        [(symbol? val) (string-quote (symbol->string val))]
        [else (raise-argument-error 'csv-quote
                                    "known kind of value"
                                    0 val)]))

;; given a list, format as a single line of CSV text
(define (csv-row->str row)
  (apply
      string-append
      (add-between
       (map csv-quote row)
       ",")))

(module+ test
  (require rackunit)
  (check-equal? (csv-row->str '(3 "has, commas," abc #f))
                "3,\"has, commas,\",\"abc\",FALSE"))



(module+ test
(check-equal? (csv->string '((a b) (c d)))
              "\"a\",\"b\"\n\"c\",\"d\"\n"))

