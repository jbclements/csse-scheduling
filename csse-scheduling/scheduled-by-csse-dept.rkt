#lang typed/racket/base

;; this file has a bunch of useful functions, but it doesn't
;; really do what it used to do, so its raison d'etre is kind
;; of broken.

(provide
 ;; this is out-of-date. Try to replace it with non-supervisory-computing-courses
 courses-we-schedule ; used?
 non-supervisory-computing-courses
 ;; includes any courses cross-listed as csc:
 non-supervisory-csc-courses
 non-supervisory-cpe-courses
 non-supervisory-ee-courses
 ee-scheduled-courses ; used?
 supervisory-courses ; used? yep.
 csc-or-cpe
 cycle-course-configuration
 cycle-course-wtus
 cycle-course-wtus/noerror

 2021-course-set
 )

;; cycles before this can be ignored for the purposes of determining
;; a subject for a number...
(define earliest-cycle-cutoff : CatalogCycle "2015-2017")
;; used to determine the list of courses to be scheduled
(define current-catalog-cycle "2021-2022")

(define computing-subjects '("CSC" "CPE" "SE" "EE"))

(define-type Configuration String)

(require "types.rkt"
         "canonicalize.rkt"
         "qtr-math.rkt"
         "credentials.rkt"
         racket/set
         racket/list
         racket/match)

(require/typed "fetch-mapping.rkt"
               [courses-we-schedule/db
                (Setof Course-Id)]
               [cycle-course-configurations
                (Listof (Pair CatalogCycle
                              (Listof (Pair Course-Id Configuration))))]
               [course-mappings
                (Listof Course-Mapping)])

(define-type Course-Mapping (Vector CatalogCycle String String Course-Id))
(define (mapping-cycle [cm : Course-Mapping]) : CatalogCycle
  (vector-ref cm 0))
(define (mapping-subject [cm : Course-Mapping]) : String
  (vector-ref cm 1))
(define (mapping-num [cm : Course-Mapping]) : String
  (vector-ref cm 2))
(define (mapping-id [cm : Course-Mapping]) : Course-Id
  (vector-ref cm 3))

;; 2020-06-26 not sure whether *any* of these lists of courses are used any more.

;; NOTE: it would probably be much more robust to list the ones
;; that we *don't* schedule. That way, new courses will by default
;; be listed in the yes column.
(define ee-scheduled-courses : (Setof String)
  (list->set
   (map
    ensure-canonical
    '("cpe100" ; "Computer Engineering Orientation"
      "cpe133" ; "Digital Design"
      "cpe233" ; "Computer Design and Assembly Language Programming"
      "cpe316" ; microcontroller mumble
      "cpe328" ; "Discrete Time Signals and Systems"
      "cpe329" ; "Programmable Logic and Microprocessor-Based Systems Design"
      "cpe336" ; "Microprocessor System Design"
      "cpe368" ; "Signals and Systems Laboratory"
      "cpe428" ; "Computer Vision"
      "cpe432" ; "Digital Control Systems"
      "cpe439" ; "Introduction to Real-Time Operating Systems"
      "cpe441" ; "Computer-Aided Design of VLSI Devices"
      "cpe472" ; "Digital Control Systems Laboratory"
      "cpe488" ; "Microelectronics and Electronics Packaging"
      "cpe521" ; "Computer Systems"
      "cpe522" ; "Advanced Real-Time Operating Systems Design"
      "cpe523" ; "Digital Systems Design"
      ))))

;; should this be parameterized by the catalog cycle? sigh...

;; these are the courses we schedule. It's not entirely clear
;; to me where this is used aside from in csc-or-cpe below, which
;; allows me to write in the schedule e.g. "430" instead of "csc430"
;; I believe this list is now mostly unused.

;; NOTE: THIS LIST MUST BE IN SYNC WITH THE ONE IN THE DATABASE.
;; (you should get an error message from the check below if not.)
;; the list of courses we schedule is not quite as straightforward
;; as it could be. See the notes in "which-ones-are-ours.rkt"
;; for more discussion of this. In fact, it seems like the best
;; way to manage this is to just write them down explicitly. Like
;; this:
(define courses-we-schedule : (Setof String)
  (list->set
   (map
    ensure-canonical
    (map
     (inst first String (Listof String))
     '(("csc101" "Fundamentals of Computer Science")
       ("csc105" "Fundamentals of Computer Science I Supplemental Instruction")
       ("csc108" "Accelerated Introduction to Computer Science")
       ("csc121" "Computing for All 1")
       ("csc123" "Introduction to Computing")
       ("csc171" "Introduction to Interactive Entertainment")
       ("csc202" "Data Structures")
       ("csc203" "Project-Based Object-Oriented Programming and Design")
       ("csc209" "Problem Solving with Computers")
       ("csc225" "Introduction to Computer Organization")
       ("csc231" "Programming for Engineering Students")
       ("csc232" "Computer Programming for Scientists and Engineers")
       ("csc234" "C and Unix")
       ("csc235"
        "Fundamentals of Computer Science for Scientists and Engineers I")
       ("csc236"
        "Fundamentals of Computer Science for Scientists and Engineers II")
       ("csc290" "Selected Topics")
       ("csc300" "Professional Responsibilities")
       ("csc301" "Personal Software Process")
       ("csc302" "Computers and Society")
       ("csc303" "Teaching Computer Science")
       ("csc305" "Individual Software Design and Development")
       ("csc307" "Introduction to Software Engineering")
       ("csc308" "Software Engineering I")
       ("csc309" "Software Engineering II")
       ("csc310" "Computers for Poets")
       ("csc311" "Computational Art")
       ("csc313" "Teaching Computing")
       ("csc320" "Practical Computer Security for Everyone")
       ("csc321" "Introduction to Computer Security")
       ("csc323" "Cryptography Engineering")
       ("csc325" "Introduction to Privacy: Policy and Technology")
       ("csc344" "Music Programming")
       ("csc348" "Discrete Structures")
       ("csc349" "Design and  Analysis of Algorithms")
       ("csc350" "Computing for Interactive Arts Capstone I")
       ("csc357" "Systems Programming")
       ("csc365" "Introduction to Database Systems")
       ("csc366" "Database Modeling, Design and Implementation")
       ("csc369" "Introduction to Distributed Computing")
       ("csc371" "Game Design")
       ("csc377" "Introduction to Mixed Reality")
       ("csc378" "Interactive Entertainment Engineering")
       ("csc402" "Software Requirements Engineering")
       ("csc405" "Software Construction")
       ("csc406" "Software Deployment")
       ("csc409" "Current Topics in Software Engineering")
       ("csc410" "Software Evaluation")
       ("csc422" "Network and Web Security")
       ("csc424" "Software Security")
       ("csc429" "Current Topics in Computer Security")
       ("csc430" "Programming Languages I")
       ("csc431" "Programming Languages II")
       ("csc435"
        "Introduction to Object Oriented Design Using Graphical User Interfaces")
       ("csc436" "Mobile Application Development")
       ("csc437" "Dynamic Web Development")
       ("csc445" "Theory of Computation I")
       ("csc448" "Bioinformatics Algorithms")
       ("csc450" "Computing for Interactive Arts Capstone II")
       ("csc453" "Introduction to Operating Systems")
       ("csc454" "Implementation of Operating Systems")
       ("csc458" "Current Topics in Computer Systems")
       ("csc466" "Knowledge Discovery from Data")
       ("csc468" "Database Management Systems Implementation")
       ("csc469" "Distributed Systems")
       ("csc471" "Introduction to Computer Graphics")
       ("csc473" "Advanced Rendering Techniques")
       ("csc474" "Computer Animation")
       ("csc476" "Real-Time 3D Computer Graphics Software")
       ("csc477" "Scientific and Information Visualization")
       ("csc478" "Current Topics in Computer Graphics")
       ("csc480" "Artificial Intelligence")
       ("csc481" "Knowledge Based Systems")
       ("csc482" "Speech and Language Processing")
       ("csc483" "Current Topics in Human-Computer Interaction")
       ("csc484" "User-Centered Interface Design and Development")
       ("csc486" "Human-Computer Interaction Theory and Design")
       ("csc487" "Deep Learning")
       ("csc489" "Current Topics in Artificial Intelligence")
       ("csc490" "Selected Advanced Topics")
       ("csc496" "Selected Advanced Laboratory")
       ("csc508" "Software Engineering I")
       ("csc509" "Software Engineering II")
       ("csc515" "Computer Architecture")
       ("csc521" "Computer Security")
       ("csc530" "Languages and Translators")
       ("csc540" "Theory of Computation II")
       ("csc549" "Advanced Algorithm Design and Analysis")
       ("csc550" "Operating Systems")
       ("csc560" "Database Systems")
       ("csc564" "Computer Networks: Research Topics")
       ("csc566" "Topics in Advanced Data Mining")
       ("csc569" "Distributed Computing")
       ("csc570" "Current Topics in Computer Science")
       ("csc572" "Computer Graphics")
       ("csc580" "Artificial Intelligence")
       ("csc581" "Computer Support for Knowledge Management")
       ("csc582" "Introduction to Natural Language Processing")
       ("csc590" "Thesis Seminar")
       ("cpe290" "Selected Topics")
       ("cpe315" "Computer Architecture")
       ("cpe350" "Capstone I")
       ("cpe416" "Autonomous Mobile Robotics")
       ("cpe419" "Applied Parallel Computing")
       ("cpe450" "Capstone II")
       ("cpe464" "Introduction to Computer Networks")
       ("cpe465" "Advanced Computer Networks")
       ("cpe470" "Selected Advanced Topics")
       ("cpe479" "Selected Advanced Laboratory")
       ("cpe482" "Advanced Topics in Systems for Computer Engineering")
       ("cpe485" "Autonomous Robot Navigation")
       ;; extras from the past:
       ("csc102")
       ("csc103"))))))

;; the list of courses in the 


(unless (equal? courses-we-schedule
                courses-we-schedule/db)
  (error 'courses-we-schedule
         "database doesn't match local list: missing ~e, extra ~e"
         (set-subtract courses-we-schedule
                       courses-we-schedule/db)
         (set-subtract courses-we-schedule/db courses-we-schedule)))

;; these are supervisory courses, so they aren't scheduled
;; by the scheduler
;; these could be checked using FAD data, after the fact. maybe that's a FIXME
(define supervisory-courses : (Setof String)
  (list->set
   (map
    ensure-canonical
    '("cpe200"
      "cpe400"
      "cpe461"
      "cpe462"
      "cpe493"
      "cpe494"
      "cpe495"
      "csc200"
      "csc400"
      "csc491"
      "csc492"
      "csc497"
      "csc498"
      "csc493"
      "csc494"
      "csc495"
      "csc500"
      "csc593"
      "csc594"
      "csc595"
      "csc596"
      "csc597"
      "csc599"
      "ee200"
      "ee400"
      "ee461"
      "ee462"
      "ee463"
      "ee464"
      "ee493"
      "ee494"
      "ee495"
      "ee500"
      "ee594"
      "ee595"
      "ee599"))))

;; return non-supervisory courses that have one of these subjects
(define (non-sup-courses-in-subjects [subjects : (Listof String)])
  : (Setof Course-Id)
  (set-subtract
   (list->set
    (map
     mapping-id
     (filter
      (λ ([cm : Course-Mapping])
        (and (equal? (mapping-cycle cm) current-catalog-cycle)
             (member (mapping-subject cm) subjects)))
      course-mappings)))
   supervisory-courses))

(define non-supervisory-computing-courses
  (non-sup-courses-in-subjects computing-subjects))
(define non-supervisory-csc-courses
  (non-sup-courses-in-subjects '("CSC")))
(define non-supervisory-cpe-courses
  (non-sup-courses-in-subjects '("CPE")))
(define non-supervisory-ee-courses
  (non-sup-courses-in-subjects '("EE")))

;; for 2021-2022 planning, I'm going to add together
;; the csc courses and the CPE courses that aren't cross
;; listed with ee. Yikes.
(define cpe-extra
  (set-subtract non-supervisory-cpe-courses
                (set-union non-supervisory-csc-courses
                           non-supervisory-ee-courses)))
;; this is pretty temporary...
(define 2021-course-set (set-union non-supervisory-csc-courses cpe-extra))

;; map numbers to ids to allow short-cuts in schedule description.
;; for instance, we can just write "430" rather than "csc430".
(define num-id-table
  (let ()
    
    (define (newer-than-cutoff [cc : CatalogCycle])  : Boolean
      (not (catalog-cycle-<? cc earliest-cycle-cutoff)))
    (define csc-cpe-mappings
      (filter (λ ([cm : Course-Mapping])
                (and
                 (member (mapping-subject cm) '("CSC" "CPE"))
                 (newer-than-cutoff (mapping-cycle cm))))
              course-mappings))
    (make-immutable-hash
     (map (λ ([ms : (Listof Course-Mapping)])
            (ann (cons (mapping-num (first ms))
                       (remove-duplicates
                        (map mapping-id ms)))
                 (Pairof String (Listof Course-Id))))
          (group-by mapping-num csc-cpe-mappings)))))

(module+ test
  (check-equal? (hash-ref num-id-table "100") '("cpe100"))
  (check-equal? (hash-ref num-id-table "350") '("csc350" "cpe350")))

;; determine the canonical name from just the number, when possible.
(: csc-or-cpe (Natural [#:noerr Boolean] -> (U String False)))
(define (csc-or-cpe coursenum #:noerr [noerr? #f])
  (define hits (hash-ref num-id-table (number->string coursenum) (λ () '())))
  (match hits
    [(list) (cond [noerr? #f]
                  [else (error 'csc-or-cpe "no courses in CSC or CPE that we schedule with number ~e"
                               coursenum)])]
    [(list name) name]
    [(list _ ...)
     (cond [noerr? #f]
           [else (error 'csc-or-cpe
                        "more than one hit (~e) for course number: ~e"
                        hits coursenum)])]))

;; given a course and a catalog cycle, return its configuration string
(define (cycle-course-configuration [course : Course-Id] [cycle : CatalogCycle]) : (U False Configuration)
  (define configurations
    (match (assoc cycle cycle-course-configurations)
      [(cons _ c) c]
      [#f (error 'cycle-course-configuration
                 "no configuration info for cycle ~v" cycle)]))
  (match (assoc course configurations)
    [#f #f]
    [(cons id configuration) configuration]))

(define-predicate false? False)

;; given a course, return the number of WTUs required to teach it
;; in the 2019-2021 catalog, or return #f if it's nonstandard
(define (cycle-course-wtus/noerror [cycle : CatalogCycle] [course : Course-Id] [lab-mult : Natural 1])
  : (U False Nonnegative-Exact-Rational)
  (define maybe-configuration (cycle-course-configuration course cycle))
  (cond
    [(not maybe-configuration) (error 'cycle-course-wtus "no mapping found for course: ~e" course)]
    [else
     (define maybe-wtus (configuration->wtus maybe-configuration lab-mult))
     (cond [maybe-wtus maybe-wtus]
           [else #f])]))

;; same as above, but signal an error if nonstandard
(define (cycle-course-wtus [cycle : CatalogCycle] [course : Course-Id] [lab-mult : Natural 1])  : Nonnegative-Exact-Rational
   (or (cycle-course-wtus/noerror cycle course lab-mult)
       (error 'cycle-course-wtus
              "nonstandard configuration for course: ~e"
              course)))

(define lecture-unit-wtus : Positive-Exact-Rational #e1.0)
(define lab-unit-wtus : Positive-Exact-Rational #e2.0)
(define activity-unit-wtus : Positive-Exact-Rational #e1.3)

;; return the number of wtus associated with a course. For mega-sections, we multiply
;; the lab WTUs, on the assumption that mega-sections still have normal-sized labs
(define (configuration->wtus [c : Configuration] [lab-mult : Natural]) : (U False Nonnegative-Exact-Rational)
  (match c
    [(regexp #px"^([0-9]+)-([0-9]+)-([0-9]+)$"
             (list _ lectures labs activities))
     ;; casts should all succeed by regexps in pattern
     (+ (* lecture-unit-wtus      (cast (string->number (cast lectures String))   Nonnegative-Exact-Rational))
        (* lab-unit-wtus lab-mult (cast (string->number (cast labs String))       Nonnegative-Exact-Rational))
        (* activity-unit-wtus     (cast (string->number (cast activities String)) Nonnegative-Exact-Rational)))]
    [other #f]))


(module+ test
  (require typed/rackunit)

  
  (check-equal? (csc-or-cpe 123) "csc123")
  (check-exn #px"more than one hit"
             (λ () (csc-or-cpe 290)))
  (check-equal? (csc-or-cpe 431) "csc431")
  (check-equal? (csc-or-cpe 315) "cpe315")

  (check-equal? (cycle-course-wtus (ann "2019-2020" CatalogCycle) "csc101") 5)
  (check-equal? (cycle-course-wtus (ann "2019-2020" CatalogCycle) "csc232") #e3.3))


