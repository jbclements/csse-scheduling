#lang racket

(require html-parsing
         explorer
         sxml
         )

(define sxml
(call-with-input-file
    "/tmp/abc.html"
    #;"/tmp/Computer Engineering – Spring Quarter 2025 – CourseLeaf_CLSS.html"
  html->xexp
 ))

;; fragile much?
(define the-rows
  ((sxpath '(// (div (@ id (equal? "offering-grid"))) table tbody tr td div))
   sxml))

(explore the-rows)

(map second the-rows)


