#lang typed/racket

(require "course-listings.rkt"
         "canonicalize.rkt")

(provide simple-group-courses)
;; some labels simply denote a choice of several classes.

(define-type GC-Pr (Pair (List Symbol)
                         (Listof Course-Id)))

;; these are the labels that don't depend on the catalog cycle
(define simple-group-courses
  : (Listof GC-Pr)
  (list
   (cons '(232/234/235) (list "csc232" "csc234" "csc235"))
   (cons '(CIA-ELEC) (list "csc371" "csc378" "csc480" "csc481"))
   (cons '(csc-SE) (list "csc307" "csc308"))
   (cons '(cpe-arch) (list "cpe315" "cpe333"))
   (cons '(cpe-sp-1) (list "cpe461" "csc497"))
   (cons '(cpe-sp-2) (list "cpe462" "csc498"))
   (cons '(csc-sp-1) (list "csc491" "csc497"))
   (cons '(csc-sp-2) (list "csc492" "csc498"))
   (cons '(CSCMSTHESIS) (list "csc497" "csc498"
                              "csc596" "csc597" "csc599"))
   (cons '(microcon) (list "cpe329" "cpe316" "cpe336"))
   (cons '(ethics) (list "csc300" "phil323"))
   (cons '(security) (list "csc321" "csc323" "csc325"))
   (cons '(ee-microcon) '("cpe329" "cpe336"))
   (cons '(circuits) '("ee112" "ee113"))
   (cons '(circuits-lab) '("ee143" "ime156"))
   (cons '(cpe-circuits-lab) '("ee143" "ime156" "cpe488"))
   (cons '(ee-microcon) '("cpe329" "cpe336"))
   ;; imprecise, allows 461 + 464
   (cons '(ee-sp-1) '("ee461" "ee463"))
   (cons '(ee-sp-2) '("ee462" "ee464"))
   ))

(define dontcare
  (map (λ ([vals : (Listof Course-Id)]) (map ensure-canonical vals))
       (map (inst cdr Any (Listof Course-Id)) simple-group-courses)))