#lang typed/racket

;; this file maps EMPLIDs to a random 10-byte hash, using a hash key
;; stored on the Cal Poly OneDrive. It appears that the chance of
;; a collision, for 100K students, is less than 4e-15.

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
  (apply
   string-append
   (map hexpair
        (bytes->list (subbytes
                      (sha256
                       (string->bytes/utf-8 (string-append emplid hash-text)))
                      0 NUM-BYTES)))))

(unless (equal? (emplid->hashed "01234")
                "46bcbd81d771acb20ef1")
  (error 'ohdear))


