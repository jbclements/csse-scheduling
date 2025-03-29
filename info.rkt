#lang setup/infotab

(define collection 'multi)

(define deps
  '("base"
    "db-lib"
    "typed-racket-lib"
    "with-cache"
    "csv-reading"
    "sha"
    "data-enumerate-lib"
    "csv-writing"
    "sugar"
    "rackunit-typed"

    "explorer"
    "html-parsing"
    "scramble-lib"
    "sxml"
    "threading-lib"

    ))

(define build-deps
  '("rackunit-lib"
    "racket-doc"
    "scribble-lib"
    "typed-racket-more"))


