#lang scribble/manual

@title{CSSE Scheduling: scheduling database front end}

@author[(author+email "John Clements" "clements@racket-lang.org")]

@defmodule[canonicalize]{
access to the scheduling database.

 @defproc[(canonicalize [catalog-cycle string?]
                        [subject (or string? symbol?)]
                        [number (or string? nat?)])
         string?]{
 given a quarter and a subject and a number, return the canonical
 string identifying the course.

 For instance:

 @racketblock[(canonicalize "2015-2017" "CPE" "101")]
 }

                 
@defproc[(canonicalize/qtr [qtr nat?]
                           [subject (or string? symbol?)]
                           [number (or string? nat?)])
         string?]{
 given a quarter and a subject and a number, return the canonical
 string identifying the course.

 For instance:

  @racketblock[(canonicalize/qtr 2158 "CPE" "101")]

 }
}

