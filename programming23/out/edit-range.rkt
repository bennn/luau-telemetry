#lang racket

;; clean up the hash output (hhhh) from my overview script

(require
  racket/runtime-path)

(define-runtime-path out ".")

(define te* (file->value (build-path out "typeerror.rktd")))

(define seen-code (make-hash))

(define (num->english nn)
  (for/first ((kv (in-list te*))
              #:when (= (car kv) nn))
    (hash-set! seen-code nn #true)
    (cadr kv)))

(define (unseen-code)
  (for/list ((kv (in-list te*)) #:unless (hash-ref seen-code (car kv) #f)) kv))

(define (column->english str)
  (define old? (regexp-match? #rx"OldTypeError" str))
  (define num (string->number (cadr (regexp-match #rx"TypeError([0-9]+)InEdit" str))))
  (format "~a_~a"
          (if old? "Olde" "Curr")
          (num->english num)))

(define hhhh
#hash(("Studio-Luau-TypeErrors.OldTypeError1000InEditRegion" . 556)
      ("Studio-Luau-TypeErrors.OldTypeError1001InEditRegion" . 9781)
      ("Studio-Luau-TypeErrors.OldTypeError1002InEditRegion" . 1335)
      ("Studio-Luau-TypeErrors.OldTypeError1003InEditRegion" . 28)
      ("Studio-Luau-TypeErrors.OldTypeError1004InEditRegion" . 129)
      ("Studio-Luau-TypeErrors.OldTypeError1005InEditRegion" . 2)
      ("Studio-Luau-TypeErrors.OldTypeError1007InEditRegion" . 401)
      ("Studio-Luau-TypeErrors.OldTypeError1008InEditRegion" . 12)
      ("Studio-Luau-TypeErrors.OldTypeError1010InEditRegion" . 1)
      ("Studio-Luau-TypeErrors.OldTypeError1011InEditRegion" . 500)
      ("Studio-Luau-TypeErrors.OldTypeError1012InEditRegion" . 5)
      ("Studio-Luau-TypeErrors.OldTypeError1013InEditRegion" . 28565)
      ("Studio-Luau-TypeErrors.OldTypeError1016InEditRegion" . 72)
      ("Studio-Luau-TypeErrors.OldTypeError1017InEditRegion" . 326)
      ("Studio-Luau-TypeErrors.OldTypeError1019InEditRegion" . 27)
      ("Studio-Luau-TypeErrors.OldTypeError1020InEditRegion" . 47)
      ("Studio-Luau-TypeErrors.OldTypeError1022InEditRegion" . 31)
      ("Studio-Luau-TypeErrors.OldTypeError1023InEditRegion" . 42)
      ("Studio-Luau-TypeErrors.OldTypeError1024InEditRegion" . 57)
      ("Studio-Luau-TypeErrors.OldTypeError1026InEditRegion" . 59)
      ("Studio-Luau-TypeErrors.OldTypeError1027InEditRegion" . 51)
      ("Studio-Luau-TypeErrors.OldTypeError1029InEditRegion" . 343)
      ("Studio-Luau-TypeErrors.OldTypeError1030InEditRegion" . 28)
      ("Studio-Luau-TypeErrors.OldTypeError1031InEditRegion" . 2)

      ("Studio-Luau-TypeErrors.TypeError1000InEditRegion" . 851)
      ("Studio-Luau-TypeErrors.TypeError1001InEditRegion" . 10268)
      ("Studio-Luau-TypeErrors.TypeError1002InEditRegion" . 1659)
      ("Studio-Luau-TypeErrors.TypeError1003InEditRegion" . 39)
      ("Studio-Luau-TypeErrors.TypeError1004InEditRegion" . 23)
      ("Studio-Luau-TypeErrors.TypeError1005InEditRegion" . 1)
      ("Studio-Luau-TypeErrors.TypeError1006InEditRegion" . 1)
      ("Studio-Luau-TypeErrors.TypeError1007InEditRegion" . 394)
      ("Studio-Luau-TypeErrors.TypeError1008InEditRegion" . 10)
      ("Studio-Luau-TypeErrors.TypeError1011InEditRegion" . 523)
      ("Studio-Luau-TypeErrors.TypeError1012InEditRegion" . 4)
      ("Studio-Luau-TypeErrors.TypeError1013InEditRegion" . 16007)
      ("Studio-Luau-TypeErrors.TypeError1016InEditRegion" . 77)
      ("Studio-Luau-TypeErrors.TypeError1017InEditRegion" . 355)
      ("Studio-Luau-TypeErrors.TypeError1019InEditRegion" . 25)
      ("Studio-Luau-TypeErrors.TypeError1020InEditRegion" . 58)
      ("Studio-Luau-TypeErrors.TypeError1022InEditRegion" . 43)
      ("Studio-Luau-TypeErrors.TypeError1023InEditRegion" . 26)
      ("Studio-Luau-TypeErrors.TypeError1024InEditRegion" . 58)
      ("Studio-Luau-TypeErrors.TypeError1026InEditRegion" . 45)
      ("Studio-Luau-TypeErrors.TypeError1027InEditRegion" . 43)
      ("Studio-Luau-TypeErrors.TypeError1029InEditRegion" . 383)
      ("Studio-Luau-TypeErrors.TypeError1030InEditRegion" . 30)
      ("Studio-Luau-TypeErrors.TypeError1031InEditRegion" . 1)
      ("TooComplexErrors" . 26)))

(define eng*
  (for/list (((kk vv) (in-hash hhhh))
             #:when (string-prefix? kk "Studio"))
    (list (column->english kk) vv)))

(define-values [olde* curr*]
  (partition (lambda (kv) (string-prefix? (car kv) "Olde")) eng*))

(define (nsort xx)
  (sort xx > #:key cadr))

(define (output! olde*)
  (pretty-write (nsort olde*))
  (printf "~a total~n~n" (apply + (map cadr olde*))))

(output! olde*)
(output! curr*)

(let ((uc (unseen-code)))
  (printf "unseen ~a~n len ~a~n" uc (length uc)))


