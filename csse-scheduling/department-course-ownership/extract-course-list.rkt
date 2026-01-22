#lang racket

(require html-parsing
         explorer
         sxml
         )

;; this file extracts course names from the CLSS course lists. To obtain the full
;; list for (e.g.) the CPE department, go to the URL
;; https://nextcatalog-admin.calpoly.edu/wen/2254/184-CPE/?osu=184-CPE
;; (side note, I see this is under nextcatalog, I could probably script marionette here...)
;; then click "show courses with no sections", then save the page as HTML, then
;; use the code below.

(define dept
  "Statistics"
  ;"Electrical Engineering"
  #;"Computer Engineering"
  #;"Computer Science and Software Engineering"
  )

(define filename (~a dept " – Fall 2026 – CourseLeaf_CLSS.html"))

(define sxml
(call-with-input-file (build-path "/tmp" filename)
  #;"/tmp/Computer Engineering – Spring Quarter 2025 – CourseLeaf_CLSS.html"
  html->xexp
 ))

;; This is SO FRAGILE
(define the-rows
  ((sxpath '(// (div (@ id (equal? "offering-grid"))) table tbody tr td div))
   sxml))

;(explore the-rows)

(map second the-rows)


