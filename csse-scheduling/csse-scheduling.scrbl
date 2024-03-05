#lang scribble/manual

@title{CSSE Scheduling}

@author[(author+email "John Clements" "clements@racket-lang.org")]

@(require (for-label racket))

@defmodule[csse-scheduling]{The main require. Actually, maybe don't require
this one?}

@defmodule[qtr-math]{functions for mapping Cal Poly's goofy quarter system
to and from other representations.}

@defproc[(qtr->year [qtr Natural]) Natural]{
 Given a quarter such as 2188, return the corresponding year (in this case,
 2018).
}

@defproc[(qtr->season [qtr Natural]) String]{
 Given a quarter such as 2188, return the corresponding season (in this case,
 "Fall").
}

@defproc[(encode-qtr [year Natural] [season String]) Natural]{
 Given a year such as 2004 and a season such as "Winter", return
 the corresponding quarter number (in this case, 2042).
}

@defproc[(season-after-qtr [season String] [qtr Natural]) Natural]{
  Given a season such as "Spring" and a quarter such as 2188, return the
  first quarter on or after the given quarter of the given season (in
  this case, 2194).
}

@defproc[(fall-year->catalog-cycle [year Natural]) String]{
 Given a year such as 2017, return a catalog cycle such as "2017-2019".
 Note that the "fall" part is important, since, for instance, Spring 2017
 maps to the "2015-2017" catalog.}

@defproc[(catalog-cycle->fall-years ...) ...]{}
@defproc[(fall-year->base-qtr ...) ...]{}
@defproc[(qtr->fall-year ...) ...]{}
@defproc[(qtr->catalog-cycle ...) ...]{}
@defproc[(qtrs-in-range ...) ...]{}
@defproc[(qtr->string ...) ...]{}
@defproc[(fall-year->qtrs ...) ...]{
 Given a years such as 2017, return the quarters in that year (2178, 2182, 2184).
}
@defproc[(catalog-cycle->qtrs ...) ...]{}

@defproc[(next-qtr [qtr Natural]) Natural]{
  Returns the cal poly quarter number following the given one.}

@defproc[(prev-qtr [qtr Natural]) Natural]{
  Returns the cal poly quarter number preceding the given one.}

@defproc[(next-qtr/no-summer [qtr Natural]) Natural]{
  Returns the cal poly quarter number following the given one,
  unless it's a summer, in which case jump two later.}

@defproc[(prev-qtr/no-summer [qtr Natural]) Natural]{
  Returns the cal poly quarter number preceding the given one,
  unless it's a summer, in which case jump two earlier.}


@defmodule[types]{
Just dumping this here...}

@verbatim{

(define-type Qtr Natural)
(define-type Course-Id String)
(define-type Grade String)
(define-type Units Real)
(define-type Grade-Record (List Qtr Course-Id Units Grade))

(define gr-qtr : (Grade-Record -> Qtr) first)
(define gr-course : (Grade-Record -> Course-Id) second)
(define gr-units : (Grade-Record -> Units) third)
(define gr-grade : (Grade-Record -> String) fourth)

;; the name of a graduation-requirement
;; ... combine with Course-Or-Group
(define-type ReqName Course-Or-Group)

(define-type Major-Abbr (U "CSC" "CPE" "SE"))

(define-type Course-Or-Group (U Course-Id (List Symbol)))
 }

@defstruct[Seat-Requirement ([label Category]
                          [course Course-Or-Group] ;; what course do they want?
                          [qtr-req (U Natural #f)] ;; when do they want it?
                          [seats Real])]{
Represents a requirement for a certain number of seats. The @racket[label] indicates
 which population of students we're discussing, the @racket[course] says what course
 or course group they need, the @racket[qtr-req] indicates which quarter they need it
 in (if there's a specific quarter associated with the requirement), and the @racket[seats]
 indicates how many seats are needed. Note that the number of seats can easily be a non-integer.
 When a requirement is spread across multiple quarters ("take this course in either of these
 two quarters..."), the model represents this as a half-seat requirement in each quarter.
}

@defthing[Seats-By-Requirement type? #:value (Listof (List ReqName Real))]{
 An association list mapping requirement names to a count of seats.
}



@verbatim{
;; a requirement for a number of sections
(struct Section-Requirement ([label : Category] ;; which requirement is this? (used for prioritizing)
                             [course : Course-Or-Group] ;; what course do they want?
                             [qtr-req : (U Natural #f)] ;; when do they want it?
                             [sections : Real])  ;; how many do they want?
  #:transparent)

;; a category essentially represents a population, used to tag
;; a requirement
;; NOTE: there's definitely overlap. csc-bs and csc-bs-firstyear overlap,
;; and so do csc-bs and csse-cia-minor
(define-type Category (U 'csc-bs 'cpe-bs 'se-bs 'ee-bs 'bmed-bs
                         'enve-bs 'gene-bs 'ime-bs 'mate-bs 'me-bs
                         'csc-bs-firstyear 'cpe-bs-firstyear 'se-bs-firstyear
                         'csc-ms
                         
                         'laes-csc-bs

                         'stat-bs
                         'math-bs

                         'arce-bs
                         'brae-bs

                         'grc-bs

                         'msci-bs
                         'chem-bs

                         'econ-bs

                         'cs-minor
                         
                         ;; warning! overlaps with other groups:
                         'csse-cia-minor
                         'csse-data-minor

                         'non-csse-data-minor
                         'non-csse-cia-minor
                         ;; laes?
                         ;; represents a once-only load 
                         (List 'one-off Real)
                         ))}


@defmodule[canonicalize]{
Just dumping this here...}

@verbatim{
(provide canonicalize
         canonicalize/qtr
         canonicalize/noerr
         canonicalize/qtr/noerr
         canonical-id?
         ensure-canonical
         courses-in-subject
}

@defproc[(canonicalize [cc CatalogCycle] [subject (U Symbol String)] [number (U Natural CourseNum)]) Course-Id]{
   Returns the course id to which this subject and number are mapped in the given catalog cycle.
}

@defproc[(ensure-canonical [id String]) String]{
Ensure the given id is a canonical id. Signal
an error if it isn't. return it if it is.
}

@defproc[(course-key [course String]) String]{
  Given a course-id, return a string suitable for sorting
  with @racket[string<?]. This defines the standard
  sort order for course ids.
}

@verbatim{
         course-key
         id-mappings
         Subject
         CourseNum
         Course-Id
         subject?
         mappings
         MappingRow
         CatalogCycle)}

@defmodule[progress/seats-required-model]{Functions to examine individual
students to predict how many seats will be required in each course or group-course.}

@defproc[(seat-requirements/range [model-qtr Qtr]
                                 [start-qtr Qtr]
                                 [stop-qtr Qtr]
                                 [cc CatalogCycle]
                                 [omit-first-year? Boolean])
         (Listof (Listof Seat-Requirement))]{
Given a model qtr (e.g. 2202, indicating data obtained after winter 2020),
and a 'start' qtr and a 'stop' qtr, return a list of Seat-Requirement's
indicating the requirements for each modeled quarter, starting in the given start
qtr and ending one before the given stop qtr. So, for instance,
start 2208 and stop 2214 would model two quarters, fall 2020 and
winter 2021.

Some courses are highly depended-on, like 202, 203, and 357.
When students need these courses in a particular quarter of modeling,
these requirements should be tagged with the appropriate quarter,
so that we can see that they don't just need them any old time,
they need them in the appropriate quarter.
}

@defproc[(seat-requirements-reduce [losr : (Listof Seat-Requirement)])
         (Listof Seat-Requirement)]{
 Combine @racket[Seat-Requirement]s where the label, course, and quarter are the same.
}


@defproc[(student->courses [student Student]
                          [start-qtr-idx Natural]
                          [stop-qtr-idx Natural]
                          [cc CatalogCycle])
  (Listof Seats-By-Requirement)]{
Computes the courses a student is expected to take, grouped by quarter. Note that
the @racket[start-qtr-idx] and @racket[stop-qtr-idx] are zero-based indexes. So, for instance,
using 0 and 4 would tell you what the student is expected to take over the next four quarters.
}

