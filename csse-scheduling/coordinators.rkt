#lang typed/racket

;; this file defines the coordinators for CSSE courses.
;; specifically, it maps canonical course names to a
;; list of instructor last names. Courses with no coordinator
;; are associated with empty lists.

(provide coordinators)

(define-type CoordinatorRecord (List String (Listof String)))
(define-type CoordinatorList (Listof CoordinatorRecord))

;;> ooh, in the middle of moving this chunk over from the scraper to the live part.

(define coordinator-table
  (for/list ([l (in-list cleaned-lines)])
    (define canonical-name
      (match (string-trim (first l))
        ;; 2 ad-hoc fixups:
        ["CSC 419" (canonicalize current-catalog "CPE" "419")]
        ["CS 448" (canonicalize current-catalog "CSC" "448")]
        [(regexp #px"^([A-Z]+)(/[A-Z]+)? ([0-9]{3})$" (list _ subj _ num))
         (canonicalize current-catalog subj num)]))
    (define coordinator
      (match l
        [(list _ _ name)
         (regexp-split #px"/" name)]
        [(list _ _ "") #f]))
    (list canonical-name coordinator)))

(call-with-output-file "/tmp/coordinators.rktd"
  (λ (port)
    (pretty-write coordinator-table port))
  #:exists 'truncate)



(define coordinators : CoordinatorList
  '(("csc101" ("Keen"))
    ("csc108" ("Staley"))
    ("csc123" ())
    ("csc171" ("Haungs"))
    ("csc202" ("Clements"))
    ("csc203" ("Wood"))
    ("csc225" ("Workman"))
    ("csc231" ("Voelker"))
    ("csc232" ("Voelker"))
    ("csc234" ("Voelker"))
    ("csc235" ("Voelker"))
    ("csc300" ("Turner"))
    ("csc301" ())
    ("csc302" ("Turner"))
    ("csc303" ("Staley"))
    ("csc305" ("Bahrami"))
    ("csc307" ("Falessi"))
    ("csc308" ("Falessi"))
    ("csc309" ("Falessi"))
    ("csc310" ())
    ("csc311" ("Wood"))
    ("cpe315" ("Seng"))
    ("csc320" ("Peterson"))
    ("csc321" ())
    ("csc323" ("Peterson"))
    ("csc325" ("DeBruhl"))
    ("csc344" ("Clements"))
    ("csc348" ("Vakalis"))
    ("csc349" ("Gharibyan"))
    ("csc350" ("Wood"))
    ("csc357" ("Nico"))
    ("csc365" ("Dekhtyar"))
    ("csc366" ("Dekhtyar"))
    ("csc369" ("Stanchev" "Dekhtyar"))
    ("csc371" ("Haungs"))
    ("csc378" ("Khosmood"))
    ("csc402" ("Janzen"))
    ("csc405" ("Janzen"))
    ("csc406" ("Janzen"))
    ("csc410" ("Dekhtyar"))
    ("cpe419" ("Lupo"))
    ("csc422" ("DeBruhl"))
    ("csc424" ())
    ("csc430" ("Keen"))
    ("csc431" ("Keen"))
    ("csc435" ("Staley"))
    ("csc436" ("Bellardo"))
    ("csc437" ("Staley"))
    ("csc445" ("Gharibyan" "Vakalis"))
    ("csc448" ("Dekhtyar"))
    ("csc450" ("Wood"))
    ("csc453" ("Nico"))
    ("csc454" ("Bellardo"))
    ("cpe464" ("Smith"))
    ("cpe465" ("Bellardo"))
    ("csc466" ("Stanchev" "Dekhtyar"))
    ("csc468" ("Dekhtyar"))
    ("csc469" ())
    ("csc471" ("Wood"))
    ("csc473" ("Wood"))
    ("csc474" ("Wood"))
    ("csc476" ("Wood"))
    ("csc477" ())
    ("csc480" ("Kurfess"))
    ("csc481" ("Kurfess"))
    ("csc484" ("Kurfess"))
    ("csc486" ("Kurfess"))
    ("csc508" ())
    ("csc509" ())
    ("csc515" ("Lupo"))
    ("csc521" ())
    ("csc530" ("Clements"))
    ("csc540" ("Gharibyan" "Vakalis"))
    ("csc550" ("Bellardo"))
    ("csc560" ("Stanchev"))
    ("csc564" ("Bellardo"))
    ("csc566" ("Dekhtyar"))
    ("csc569" ())
    ("csc572" ("Wood"))
    ("csc580" ("Kurfess"))
    ("csc581" ("Kurfess"))
    ("csc582" ("Khosmood"))
    ("csc590" ("Dekhtyar"))))

;; make sure that all the courses mentioned are canonical names:

(require "canonicalize.rkt")

(for ([cr (in-list coordinators)])
  (ensure-canonical (first cr)))
