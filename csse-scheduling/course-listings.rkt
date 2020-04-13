#lang typed/racket

(provide (all-defined-out))
#;(provide csc-te-course-table
         csc-ul-te-course-table
         se-te-course-table
         se-ul-te-course-table
         cpe-te-course-table)

;; these classes may be used as technical electives in the 2015-2017 catalog
(define 2015-csc-te-courses
  '("csc301"
    "csc305"  "csc309"  "csc321"  "csc323"  "csc344"  "csc358"  "csc365"  "csc366"
    "csc369"  "csc371"  "csc378"  "csc400"  "csc402"  "csc405"  "csc406"  "csc409"  "csc410"
    "cpe416"  "cpe419"  "csc424"  "csc429"  "csc435"  "csc436"  "csc437"  "csc448"  "csc454"
    "csc458"  "cpe464"  "cpe465"  "csc466"  "csc468"  "csc471"  "csc473"  "csc474"  "csc476"
    "csc477"  "csc478"  "csc479"  "csc480"  "csc481"  "csc483"  "csc484"  "cpe485"  "csc486"
    "csc489"  #;"csc490"  #;"csc496"  "csc508"  "csc509"  "csc515"  "csc521"  "csc530"  "csc540"
    "csc550"  "csc560"  "csc564"  "csc566"  "csc569"  "csc570"  "csc572"  "csc580"  "csc581"
    "csc582"  "cpe400"  "cpe428"  "cpe482"  "cpe488"  "data301"))

(define 2015-csc-ul-te-courses
  '("csc366"
    "csc402"  "csc405"  "csc406"  "csc409"  "csc410"  "cpe416"  "csc424"  "csc429"
    "csc435"  "csc437"  "csc454"  "cpe465"  "csc466"  "csc468"  "csc473"  "csc474"  "csc476"
    "csc477"  "csc478"  "csc479"  "csc481"  "csc483"  "csc484"  "csc486"  "csc489"  "csc508"
    "csc509"  "csc515"  "csc521"  "csc530"  "csc540"  "csc550"  "csc560"  "csc564"  "csc566"
    "csc572"  "csc580"  "csc581"  "csc582"))


(define 2015-se-te-courses
  '("csc301"
    "cpe315"  "csc321"  "csc323"  "csc344"  "csc358"  "csc365"  "csc366"  "csc369"
    "csc371"  "csc378"  "csc400"  "csc409"  "csc410"  "cpe416"  "cpe419"  "csc424"  "csc429"
    "csc431"  "csc435"  "csc436"  "csc437"  "csc445"  "csc448"  "csc453"  "csc454"  "csc458"
    "cpe464"  "cpe465"  "csc466"  "csc468"  "csc471"  "csc473"  "csc474"  "csc476"  "csc477"
    "csc478"  "csc479"  "csc480"  "csc481"  "csc483"  "cpe485"  "csc486"  "csc489"  #;"csc490"
    "csc508"  "csc509"  "csc515"  "csc521"  "csc530"  "csc540"  "csc550"  "csc560"  "csc564"
    "csc566"  "csc569"  "csc570"  "csc572"  "csc580"  "csc581"  "csc582"  "cpe400"  "cpe428"
    "cpe482"  "cpe488"  "data301"))

(define 2015-se-ul-te-courses
  '("csc366"
    "csc409"  "csc410"  "cpe416"  "csc424"  "csc429"  "csc431"  "csc435"  "csc437"
    "csc454"  "cpe465"  "csc466"  "csc468"  "csc473"  "csc474"  "csc476"  "csc477"  "csc478"
    "csc479"  "csc481"  "csc483"  "csc486"  "csc489"  "csc508"  "csc509"  "csc515"  "csc521"
    "csc530"  "csc540"  "csc550"  "csc560"  "csc564"  "csc566"  "csc572"  "csc580"  "csc581"
    "csc582"))


;; these classes may be used as technical electives in the 2017-2019 catalog
(define 2017-csc-te-courses
  '("csc301"
    "csc305" "csc309" "csc321" "csc323" "csc325" "csc344" "csc365"
    "csc366" "csc369" "csc371" "csc378" "csc400" "csc402" "csc405"
    "csc406" "csc409" "csc410" "csc422" "csc424" "csc429" "csc435"
    "csc436" "csc437" "csc448" "csc454" "csc458" "csc466" "csc468"
    "csc471" "csc473" "csc474" "csc476" "csc477" "csc478" "csc480"
    ;; 496 should be binned with 400/400/490
    "csc481" "csc483" "csc484" "csc486" "csc489" #;"csc496"
    "csc508" "csc509" "csc515" "csc521" "csc530" "csc540" "csc550"
    "csc560" "csc564" "csc566" "csc569" "csc570" "csc572" "csc580"
    "csc581" "csc582" "cpe400" "cpe416" "cpe419" "cpe428" "cpe464"
    "cpe465" "cpe482" "cpe485" "cpe488" "data301"))

(define 2017-csc-ul-te-courses : (Listof String)
  '("csc325"
    "csc366" "csc402" "csc405" "csc406" "csc409" "csc410"
    "csc422" "csc424" "csc429" "csc435" "csc437" "csc454"
    "csc466" "csc468" "csc473" "csc474" "csc476" "csc477"
    "csc478" "csc481" "csc483" "csc484" "csc486" "csc489"
    "csc508" "csc509" "csc515" "csc521" "csc530" "csc540"
    "csc550" "csc560" "csc564" "csc566" "csc572" "csc580"
    "csc581" "csc582" "cpe416" "cpe465"))

(define 2019-2020-csc-te-courses
  '("csc301"
    "csc305" "csc309" "csc313" "csc321" "csc323" "csc325" "csc344"
    "csc365" "csc366" "csc369" "csc371" "csc377" "csc378" "csc400" "csc402"
    "csc405" "csc406" "csc409" "csc410" "csc422" "csc424" "csc429" "csc435"
    "csc436" "csc437" "csc448" "csc454" "csc458" "csc466" "csc468" "csc469"
    "csc471" "csc473" "csc474" "csc476" "csc477" "csc478" "csc480" "csc481"
    "csc482" "csc483" "csc484" "csc486" "csc487" "csc489" "csc490" "csc496"
    "csc508" "csc509" "csc515" "csc521" "csc530" "csc540" "csc549" "csc550"
    "csc560" "csc564" "csc566" "csc569" "csc570" "csc572" "csc580" "csc581"
    "csc582" "cpe400" "cpe416" "cpe419" "cpe428" "cpe464" "cpe465" "cpe482"
    "cpe485" "cpe488" "data301"))

(define 2019-2020-csc-ul-te-courses
  '("csc325"
    "csc366" "csc402" "csc405" "csc406" "csc409" "csc410" "csc422"
    "csc424" "csc429" "csc435" "csc437" "csc448" "csc454" "csc466" "csc468"
    "csc473" "csc474" "csc476" "csc477" "csc478" "csc481" "csc482" "csc483"
    "csc484" "csc486" "csc487" "csc489" "csc508" "csc509" "csc515" "csc521"
    "csc530" "csc540" "csc549" "csc550" "csc560" "csc564" "csc566" "csc572"
    "csc580" "csc581" "csc582" "cpe416" "cpe465"))
 
(define 2017-se-te-courses : (Listof String)
  '("csc301" "csc321" "csc323" "csc325" "csc344" "csc365" "csc366"
    "csc369" "csc371" "csc378" "csc400" "csc409" "csc410" "csc422"
    "csc424" "csc429" "csc431" "csc435" "csc436" "csc437" "csc445"
    "csc448" "csc453" "csc454" "csc458" "csc466" "csc468" "csc471"
    "csc473" "csc474" "csc476" "csc477" "csc478" "csc480" "csc481"
    "csc483" "csc486" "csc489" "csc490" "csc508" "csc509" "csc515"
    "csc521" "csc530" "csc540" "csc550" "csc560" "csc564" "csc566"
    "csc569" "csc570" "csc572" "csc580" "csc581" "csc582" "cpe315"
    "cpe400" "cpe416" "cpe419" "cpe428" "cpe464" "cpe465" "cpe482"
    "cpe485" "cpe488" "data301"))

(define 2019-2020-se-te-courses
  '("csc301"
    "csc313" "csc321" "csc323" "csc325" "csc344" "csc366" "csc369"
    "csc371" "csc377" "csc378" "csc400" "csc409" "csc410" "csc422" "csc424"
    "csc429" "csc431" "csc435" "csc436" "csc437" "csc445" "csc448" "csc453"
    "csc454" "csc458" "csc466" "csc468" "csc469" "csc471" "csc473" "csc474"
    "csc476" "csc477" "csc478" "csc480" "csc481" "csc482" "csc483" "csc486"
    "csc487" "csc489" "csc490" "csc496" "csc498" "csc508" "csc509" "csc515"
    "csc521" "csc530" "csc540" "csc549" "csc550" "csc560" "csc564" "csc566"
    "csc569" "csc570" "csc572" "csc580" "csc581" "csc582" "cpe315" "cpe400"
    "cpe416" "cpe419" "cpe428" "cpe464" "cpe465" "cpe482" "cpe485" "cpe488"
    "data301"))

(define 2017-se-ul-te-courses : (Listof String)
  '("csc325"
    "csc366" "csc409" "csc410" "csc422" "csc424" "csc429" "csc431"
    "csc435" "csc437" "csc454" "csc466" "csc468" "csc473" "csc474"
    "csc476" "csc477" "csc478" "csc481" "csc483" "csc486" "csc489"
    "csc508" "csc509" "csc515" "csc521" "csc530" "csc540" "csc550"
    "csc560" "csc564" "csc566" "csc572" "csc580" "csc581" "csc582"
    "cpe416" "cpe465"))

(define 2019-2020-se-ul-te-courses : (Listof String)
  '("csc325"
    "csc366" "csc409" "csc410" "csc422" "csc424" "csc429" "csc431"
    "csc435" "csc437" "csc448" "csc454" "csc466" "csc468" "csc473" "csc474"
    "csc476" "csc477" "csc478" "csc481" "csc482" "csc483" "csc486" "csc487"
    "csc489" "csc498" "csc508" "csc509" "csc515" "csc521" "csc530" "csc540"
    "csc549" "csc550" "csc560" "csc564" "csc566" "csc572" "csc580" "csc581"
    "csc582" "cpe416" "cpe465"))

(define 2017-csc-grad-courses : (Listof String)
  '("csc508"
    "csc509" "csc515" "csc521" "csc530" "csc540" "csc550" "csc560"
    "csc564" "csc566" "csc569" "csc570" "csc572" "csc580" "csc581"
    "csc582"))

;; the courses that are legal as CPE technical electives.
;; specifically, all 300, 400 and 500-level CPE and CSC courses.
;; There are eligible EE courses, as well, but they don't
;; appear in our database.
;; this includes a bunch of required courses, too, which is weird.
(define 2017-cpe-te-courses
  (append
   '("csc405"
    "csc493" "csc486" "csc454" "csc476" "csc409" "csc581" "cpe479"
    "csc521" "cpe522" "csc406" "csc469" "csc466" "csc471" "cpe461"
    "csc437" "csc530" "cpe441" "csc429" "cpe432" "cpe493" "csc481"
    "csc560" "csc599" "cpe416" "csc468" "csc422" "csc550" "csc494"
    "csc596" "csc597" "cpe428" "csc490" "cpe494" "csc484" "csc594"
    "csc410" "csc445" "csc458" "cpe470" "csc480" "csc435" "cpe485"
    "csc430" "csc473" "cpe523" "csc572" "csc450" "cpe462" "csc448"
    "cpe419" "cpe488" "csc400" "csc593" "csc431" "csc477" "csc595"
    "csc495" "csc570" "csc489" "csc492" "cpe472" "cpe482" "csc590"
    "cpe450" "csc402" "csc453" "csc509" "csc436" "cpe495" "csc500"
    "csc424" "csc491" "csc566" "cpe465" "csc508" "csc580" "csc496"
    "csc474" "csc569" "cpe464" "csc582" "cpe439" "cpe400" "csc564"
    "cpe521" "csc515" "csc540" "csc478" "csc483")
   '("cpe315"
     "cpe328" "cpe329" "cpe336" "cpe350" "cpe368" "csc300" "csc301"
     "csc302" "csc303" "csc305" "csc307" "csc308" "csc309" "csc310"
     "csc311" "csc320" "csc321" "csc323" "csc325" "csc344" "csc348"
     "csc349" "csc350" "csc357" "csc365" "csc366" "csc369" "csc371"
     "csc378")))

(define 2019-2020-cpe-te-courses
  (map
   symbol->string
   '(csc313 csc320 csc321 csc321 csc323 csc325 csc344 csc348 csc349 csc350 csc357 csc357 csc365
            csc366 csc369 csc371 csc377 csc378 csc400 csc402 csc405 csc430 csc450 csc453 csc453 csc454
            csc454 csc458 csc458 csc466 csc468 csc469 csc469 csc471 csc471 csc473 csc474 csc476 csc476
            csc477 csc478 csc480 csc481 csc482 csc483 csc484 csc486 csc487 csc489 csc490 csc300 csc301
            csc302 csc303 csc305 csc307 csc308 csc309 csc310 csc311 csc406 csc409 csc410 csc422 csc422
            csc424 csc429 csc431 csc431 csc435 csc436 csc437 csc445 csc448 csc491 csc492 csc493 csc494
            csc495 csc496 csc497 csc498 csc500 csc508 csc509 csc515 csc515 csc521 csc530 csc540 csc550
            csc560 csc564 csc564 csc566 csc569 csc569 csc570 csc572 csc580 csc581 csc582 csc590 csc593
            csc594 csc595 csc596 csc597 csc599 cpe315 cpe316 cpe327 cpe328 cpe328 cpe329 cpe329 cpe333
            cpe336 cpe336 cpe350 cpe367 cpe368 cpe368 cpe400 cpe414 cpe414 cpe416 cpe419 cpe426 cpe428
            cpe428 cpe432 cpe432 cpe439 cpe439 cpe441 cpe441 cpe442 cpe442 cpe446 cpe446 cpe447 cpe447
            cpe450 cpe461 cpe462 cpe464 cpe465 cpe470 cpe472 cpe472 cpe479 cpe482 cpe485 cpe488 cpe493
            cpe494 cpe495 cpe521 cpe521 cpe522 cpe522 cpe523 cpe523 cpe532 cpe532 cpe541 cpe541 cpe542
            cpe542 csc549)))

(define 2017-cs-minor-courses
  '("cpe315" "cpe416" "cpe419" "cpe464" "cpe465" "cpe482" "cpe485"
             "csc300" "csc301" "csc305" "csc307" "csc308" "csc309"
             "csc321" "csc323" "csc325" "csc344" "csc348" "csc349"
             "csc365" "csc366" "csc369" "csc371" "csc378" "csc400"
             "csc402" "csc405" "csc406" "csc409" "csc410" "csc422"
             "csc424" "csc429" "csc430" "csc431" "csc435" "csc436"
             "csc437" "csc445" "csc448" "csc453" "csc454" "csc458"
             "csc466" "csc468" "csc471" "csc473" "csc474" "csc476"
             "csc477" "csc478" "csc480" "csc481" "csc483" "csc484"
             "csc486" "csc489" "csc490" "csc508" "csc509" "csc515"
             "csc521" "csc530" "csc540" "csc550" "csc560" "csc564"
             "csc566" "csc569" "csc570" "csc572" "csc580" "csc581"
             "csc582" "data301"))





(define csc-te-course-table : (Immutable-HashTable String (Listof String))
  (make-immutable-hash
   (ann (list (cons "2017-2019" 2017-csc-te-courses)
              (cons "2019-2020" 2019-2020-csc-te-courses))
        (Listof (Pairof String (Listof String))))))

;; these classes may be used as the upper-level technical elective in the
;; 2017-2019 catalog
(define csc-ul-te-course-table
  (make-immutable-hash
   (ann (list (cons "2017-2019" 2017-csc-ul-te-courses)
              (cons "2019-2020" 2019-2020-csc-ul-te-courses))
        (Listof (Pairof String (Listof String))))))

(define se-te-course-table : (Immutable-HashTable String (Listof String))
  (make-immutable-hash
   (ann (list (cons "2017-2019" 2017-se-te-courses)
              (cons "2019-2020" 2019-2020-se-te-courses))
        (Listof (Pairof String (Listof String))))))

(define se-ul-te-course-table : (Immutable-HashTable String (Listof String))
  (make-immutable-hash
   (ann (list (cons "2017-2019" 2017-se-ul-te-courses)
              (cons "2019-2020" 2019-2020-se-ul-te-courses))
        (Listof (Pairof String (Listof String))))))

(define cpe-te-course-table : (Immutable-HashTable String (Listof String))
  (make-immutable-hash
   `(("2017-2019" . ,2017-cpe-te-courses)
     ("2019-2020" . ,2019-2020-cpe-te-courses))))

(define csc-ms-500-level-course-table : (Immutable-HashTable String (Listof String))
  (make-immutable-hash
   `(("2017-2019" . ,2017-csc-grad-courses))))

(define csc-ms-open-level-course-table : (Immutable-HashTable String (Listof String))
  (make-immutable-hash
   ;; this is a pretty good proxy; it's not written down in the catalog, and
   ;; it's technically up to the advisor, I believe
   `(("2017-2019" . ,2017-cpe-te-courses))))

(define cs-minor-course-table  : (Immutable-HashTable String (Listof String))
  (make-immutable-hash
   `(("2017-2019" . ,2017-cs-minor-courses))))

