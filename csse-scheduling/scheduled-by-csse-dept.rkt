#lang typed/racket

;; this file lists all of the courses that we schedule.
;; it also provides a sort key that allows us to sort courses
;; interleaved by number.

;; when we add new courses, we need to also add them to this
;; file. We'll probably notice, though, because attempts to
;; schedule them will fail.


(provide
 courses-we-schedule
 supervisory-courses
 csc-technical-electives
 course-key
 csc-or-cpe
 2017-course-configuration)

(define-type Configuration String)

(require "canonicalize.rkt"
         "credentials.rkt")

(require/typed
 db
 [#:opaque Connection connection?]
 [postgresql-connect
  (#:port Number #:user String #:database String #:password String
   -> Connection)]
 [query-rows
  (Connection String Any * -> (Listof (Vectorof Any)))]
 [query-list
  (Connection String Any * -> (Listof Any))])


(define current-catalog : CatalogCycle "2017-2019")

;; NOTE: THIS LIST MUST BE IN SYNC WITH THE ONE IN THE DATABASE.
;; (you should get an error message if it's not...)
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
       ("csc483" "Current Topics in Human-Computer Interaction")
       ("csc484" "User-Centered Interface Design and Development")
       ("csc486" "Human-Computer Interaction Theory and Design")
       ("csc489" "Current Topics in Artificial Intelligence")
       ("csc490" "Selected Advanced Topics")
       ("csc496" "Selected Advanced Laboratory")
       ("csc508" "Software Engineering I")
       ("csc509" "Software Engineering II")
       ("csc515" "Computer Architecture")
       ("csc521" "Computer Security")
       ("csc530" "Languages and Translators")
       ("csc540" "Theory of Computation II")
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
(define courses-we-schedule/db : (Setof CourseID)
  (let ()
    (define conn
      (postgresql-connect #:port 13432
                          #:user db-username
                          #:database "scheduling"
                          #:password db-password))
    (define ids
      (query-list
       conn
       (~a "SELECT id FROM our_courses")))
    (list->set (cast ids (Listof CourseID)))))


(unless (equal? courses-we-schedule
                courses-we-schedule/db)
  (error 'courses-we-schedule
         "database doesn't match local list: missing ~e, extra ~e"
         (set-subtract courses-we-schedule courses-we-schedule/db)
         (set-subtract courses-we-schedule/db courses-we-schedule)))


;; these are supervisory courses, so they aren't scheduled
;; by the scheduler
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
      "csc493"
      "csc494"
      "csc495"
      "csc500"
      "csc593"
      "csc594"
      "csc595"
      "csc596"
      "csc597"
      "csc599"))))

(define csc-technical-electives
  (list->set
   (map ensure-canonical
        '("csc301"
          "csc305"
          "csc309"
          "csc321"
          "csc323"
          "csc325"
          "csc344"
          "csc365"
          "csc366"
          "csc369"
          "csc371"
          "csc378"
          "csc400"
          "csc402"
          "csc405"
          "csc406"
          "csc409"
          "csc410"
          "csc422"
          "csc424"
          "csc429"
          "csc435"
          "csc436"
          "csc437"
          "csc448"
          "csc454"
          "csc458"
          "csc466"
          "csc468"
          "csc471"
          "csc473"
          "csc474"
          "csc476"
          "csc477"
          "csc478"
          "csc480"
          "csc481"
          "csc483"
          "csc484"
          "csc486"
          "csc489"
          "csc490"
          "csc496"
          "csc508"
          "csc509"
          "csc515"
          "csc521"
          "csc530"
          "csc540"
          "csc550"
          "csc560"
          "csc564"
          "csc566"
          "csc569"
          "csc570"
          "csc572"
          "csc580"
          "csc581"
          "csc582"
          "cpe400"
          "cpe416"
          "cpe419"
          "cpe428"
          "cpe464"
          "cpe465"
          "cpe482"
          "cpe485"
          "cpe488"
          "data301"))))



;; defines a mapping from course ids to strings for the purposes
;; of sorting. Interleaves CSC and CPE, and other majors
;; come later
(define (course-key [course : String]) : String
  (match course
    [(regexp #px"^[a-zA-Z]+([0-9]+)" (list _ num))
     (string-append (cast num String) "-" course)]
    [other
     (string-append "UNK-" course)]))

;; map numbers to ids
(define num-id-table : (HashTable Natural (Listof String))
(for/fold ([ht : (HashTable Natural (Listof String)) (hash)])
          ([id (in-set courses-we-schedule)])
  (define num (match id
                [(regexp #px"^[a-z]+([0-9]+)" (list _ n))
                 (cast (string->number (cast n String))
                       Natural)]))
  (hash-set ht num (cons id (hash-ref ht num (λ () '()))))))

;; determine the canonical name from just the number, when possible.
;; this draws on the ids, not the offering numbers
(: csc-or-cpe (Natural [#:noerr Boolean] -> (U String False)))
(define (csc-or-cpe coursenum #:noerr [noerr? #f])
  (define hits (hash-ref num-id-table coursenum (λ () '())))
  (match hits
    [(list) (cond [noerr? #f]
                  [else (error 'csc-or-cpe "no courses with number ~e"
                               coursenum)])]
    [(list name) name]
    [(list _ ...)
     (cond [noerr? #f]
           [else (error 'csc-or-cpe
                        "more than one hit (~e) for course number: ~e"
                        hits coursenum)])]))

(define 2017-course-configurations
  : (Listof (Pair CourseID Configuration))
  (let ()
    (define conn
      (postgresql-connect #:port 13432
                          #:user "scheduler"
                          #:database "scheduling"
                          #:password "aoeuidht"))
    (map
     (λ (v)
       (define v1 (cast v (Vector String Configuration)))
       (cons (vector-ref v1 0) (vector-ref v1 1)))
     (query-rows
      conn
      "SELECT ci.id, ci.configuration
 FROM (our_courses oc INNER JOIN course_info ci
       ON oc.id = ci.id)
 WHERE ci.cycle = $1"
      "2017-2019"))))

;; given a course, return its 2017 configuration string, or
;; #f it doesn't appear in the current catalog
(define (2017-course-configuration [course : CourseID]) : (U False Configuration)
  (match (assoc course 2017-course-configurations)
    [#f #f]
    [(cons id configuration) configuration]))

(module+ test
  (require typed/rackunit)

  
  (check-equal? (csc-or-cpe 123) "csc123")
  (check-exn #px"more than one hit"
             (λ () (csc-or-cpe 290)))
  (check-equal? (csc-or-cpe 431) "csc431")
  (check-equal? (csc-or-cpe 315) "cpe315")

  
  (check-equal? (course-key "csc243") "243-csc243"))




