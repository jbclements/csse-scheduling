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

