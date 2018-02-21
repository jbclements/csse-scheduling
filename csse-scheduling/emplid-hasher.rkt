#lang typed/racket

;; this file maps EMPLIDs to a random 10-byte hash, using a hash key
;; stored on the Cal Poly OneDrive. It appears that the chance of
;; a collision, for 100K students, is less than 4e-15.

;; note that cal poly can be a little inconsistent; Emplids are sometimes
;; treated as strings, and sometimes treated as integers, in which case
;; leading zeros may be trimmed, yikes. To fix this, we pad strings
;; shorter than the expected value to 9 digits.

(provide emplid->hashed)

(require/typed sha
               [sha256 (Bytes -> Bytes)])

(define hash-text
  (string-trim
   (file->string
    "/Users/clements/OneDrive - California Polytechnic State \
University/student-data-hash/student-hash-key.txt")))

;; convert to hex string, add leading zero if necessary
(define (hexpair [n : Byte]) : String
  (define hex (number->string n 16))
  (cond [(= (string-length hex) 1) (string-append "0" hex)]
        [else hex]))

(define NUM-BYTES 10)
;; with 10 bytes, chance of collision with 100K students is
;; approximately (/ (- (/ (* 100000 100000) (expt 2 80))) (log 10))
;; => 3.59 x 10^-15.

;; I'm ignoring the possibility of collisions.

(define (emplid->hashed [emplid : String])
  (define padding-string
    (cond [(<= (string-length emplid) 9)
           (apply string-append
                  (build-list (- 9 (string-length emplid)) (λ (_) "0")))]
          [else (raise-argument-error
                 'emplid->hashed
                 "string of length <= 9"
                 0 emplid)]))
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

  (check-equal? (emplid->hashed "1234")
          (emplid->hashed "000001234")))


