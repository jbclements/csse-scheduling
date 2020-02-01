#lang scribble/manual

@title{CSSE Scheduling}

@author[(author+email "John Clements" "clements@racket-lang.org")]

@(require (for-label racket))

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
@defproc[(year->qtrs ...) ...]{}
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

;; a requirement for a number of seats
(struct Seat-Requirement ([label : Category] ;; which requirement is this? (used for prioritizing)
                          [course : Course-Or-Group] ;; what course do they want?
                          [qtr-req : (U Natural #f)] ;; when do they want it?
                          [seats : Real])  ;; how many do they want?
  #:transparent)

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
