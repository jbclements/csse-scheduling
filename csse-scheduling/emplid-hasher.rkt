#lang typed/racket

;; this file maps EMPLIDs to a random 10-byte hash, using a hash key
;; stored on the Cal Poly OneDrive. It appears that the chance of
;; a collision, for 100K students, is less than 4e-15.

;; note that cal poly can be a little inconsistent; Emplids are sometimes
;; treated as strings, and sometimes treated as integers, in which case
;; leading zeros may be trimmed, yikes. To fix this, we pad strings
;; shorter than the expected value to 9 digits.

(provide emplid->hashed)

(require "config.rkt" racket/format)
(require/typed sha
               [sha256 (Bytes -> Bytes)])



(define hash-text
  (string-trim
   (file->string
    (build-path (cast (config-env 'onedrive-directory) String) "student-data-hash/student-hash-key.txt"))))

(define bytes-hash : (Mutable-HashTable Byte Natural)
  (make-hash))

;; convert to hex string, add leading zero if necessary
(define (hexpair [n : Byte]) : String
  (hash-set! bytes-hash n (add1 (hash-ref bytes-hash n (λ () 0))))
  (define hex (number->string n 16))
  (cond [(= (string-length hex) 1) (string-append "0" hex)]
        [else hex]))

(define NUM-BYTES 10)
;; with 10 bytes, chance of collision with 100K students is
;; approximately (/ (- (/ (* 100000 100000) (expt 2 80))) (log 10))
;; => 3.59 x 10^-15.

;; I'm ignoring the possibility of collisions.

(define (emplid->hashed [emplid : String]) : String
  ;; shorter than 5 is probably an implausibly large number of leading zeros
  (when (not (regexp-match #px"^[0-9]{5,9}$" emplid))
    (error 'emplid->hashed "expected 9-digit string possibly with leading zeros trimmed, got: ~e"
           emplid))
  (define padding-string
    (apply string-append
           (build-list (- 9 (string-length emplid)) (λ (_) "0"))))
  (apply
   string-append
   (map hexpair
        (bytes->list (subbytes
                      (sha256
                       (string->bytes/utf-8
                        (string-append padding-string emplid hash-text)))
                      0 NUM-BYTES)))))

(module+ test
  (require typed/rackunit)

  (check-equal? (emplid->hashed "003911748")
                "926ae4efd422daaa2afe")
  
  (check-equal? (emplid->hashed "01234")
                "b6e25c191fb6d111cc3a")

  (check-equal? (emplid->hashed "001234")
          (emplid->hashed "000001234"))

  (check-equal? (hexpair 4) "04")
  (check-equal? (hexpair 16) "10")

  (check-exn #px"expected 9-digit string"
             (λ () (emplid->hashed "4")))
  (check-exn #px"expected 9-digit string"
             (λ () (emplid->hashed "48127a430")))
)


