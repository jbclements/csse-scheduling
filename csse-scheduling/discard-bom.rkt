#lang typed/racket

(provide discard-bom)

;; strip a leading byte order mark from an input port
(: discard-bom (Input-Port -> Input-Port))
(define (discard-bom p)
  (regexp-try-match #rx"^\uFEFF" p)
  p)

(module+ test
  (require typed/rackunit)

  
  (define in-port (open-input-string "\uFEFFhello there"))
  
  (check-equal? (read-line (discard-bom in-port)) "hello there")
  
  (define in-port2 (open-input-string "\uFEFFhello there"))
  (discard-bom in-port2)
  (check-equal? (read-line in-port2) "hello there"))