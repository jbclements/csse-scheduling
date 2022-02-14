#lang typed/racket/base

(require racket/list
         racket/set
         typed/rackunit)

;; we don't want refactoring to change existing requirements.
;; these regression tests could catch that.


(require "degree-requirements.rkt")

(check-equal?
 (list->set
  (map (inst first Any)
       (hash-ref program-requirements
                 (list '(CSC) "2017-2019"))))
 (set
  "csc225"
  '(csc-TE/123)
  '(csc-TE-1)
  '(csc-SE)
  "cpe315"
  '(csc-TE-2)
  "csc203"
  '(csc-TE/special-problems)
  "csc202"
  "csc101"
  '(csc-TE-0)
  "csc492"
  "csc491"
  "csc445"
  "csc453"
  "csc431"
  "csc430"
  '(upper-level-csc-TE)
  "csc348"
  "csc349"
  "csc357"
  "csc300"
  '(csc-TE-3))
 "csc-2017")

(check-equal?
 (list->set
  (map (inst first Any)
       (hash-ref program-requirements
                 (list '(CSC) "2019-2020"))))
 (set
 '(csc-TE-4)
 "csc225"
 '(csc-TE/123)
 '(csc-SE)
 "cpe315"
 '(csc-TE-2)
 '(csc-TE-3)
 "csc203"
 '(csc-TE/special-problems)
 '(ethics)
 "csc202"
 '(csc-sp-2)
 '(csc-TE-1)
 "csc101"
 '(csc-sp-1)
 '(csc-TE-0)
 "csc445"
 "csc453"
 "csc430"
 '(upper-level-csc-TE)
 "csc348"
 "csc349"
 "csc357"))

(check-equal?
 (list->set
  (map (inst first Any)
       (hash-ref program-requirements
                 (list '(CSC) "2020-2021"))))
 (set
  '(csc-TE-4)
  "csc225"
  '(csc-TE/123)
  '(csc-SE)
  "cpe315"
  '(csc-TE-2)
  '(csc-TE-3)
  "csc203"
  '(csc-TE/special-problems)
  '(ethics)
  "csc202"
  '(csc-sp-2)
  '(csc-TE-1)
  "csc101"
  '(csc-sp-1)
  '(csc-TE-0)
  "csc445"
  "csc453"
  "csc430"
  '(upper-level-csc-TE)
  "csc348"
  "csc349"
  "csc357")
 "csc-2020")

(check-equal?
 (list->set
  (map (inst first Any)
       (hash-ref program-requirements
                 (list '(CSC) "2021-2022"))))
 (set
  "csc225"
  '(csc-TE/123)
  '(csc-SE)
  "cpe315"
  '(security)
  '(csc-TE-2)
  '(csc-TE-3)
  "csc203"
  '(csc-TE/special-problems)
  '(ethics)
  "csc202"
  '(csc-sp-2)
  '(csc-TE-1)
  "csc101"
  '(csc-sp-1)
  '(csc-TE-0)
  "csc445"
  "csc453"
  "csc430"
  '(upper-level-csc-TE)
  "csc348"
  "csc349"
  "csc357")
 "csc-2021")

(list->set
  (map (inst first Any)
       (hash-ref program-requirements
                 (list '(CSC) "2022-2023"))))