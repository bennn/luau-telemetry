#lang racket/base

(provide (all-defined-out))

(require
  racket/runtime-path
  pict-abbrevs
  colorblind-palette
  racket/string
  racket/format
  racket/math
  racket/file
  racket/path
  racket/list
  plot/no-gui
  (only-in plot/utils linear-seq)
  (only-in gregor datetime<? datetime<=? datetime->iso8601 posix->datetime ->posix parse-datetime
           days-between ~t
           minutes-between milliseconds-between current-timezone +days datetime
           ->year ->month ->day ->hours))

;; ---

(define-runtime-path data-dir "../out")
(define-runtime-path img-dir "../img")

(current-timezone "America/Los_Angeles") ;; does this even help? times seem from the data either way :/

(define roblox-mode* '(nocheck nonstrict strict))

(define key:switchedmodule "SwitchedModule")
(define key:mode "Mode")
(define key:sessionid "SessionId")
(define key:files "Files")
(define key:lines "Lines")
(define key:edits "LinesInEditRange")
(define key:te "TypeErrors")
(define key:fs "ForcedStrictTypeErrors")
(define key:fs-mod "ForcedStrictTypeErrorsInModule")
(define key:old-fs-mod "OldForcedStrictTypeErrorsInModule")
(define key:old-te-mod "OldTypeErrorsInModule")
(define key:te-mod "TypeErrorsInModule")
(define key:te-region "TypeErrorsInEditRange")
(define key:old-te-region "OldTypeErrorsInEditRange")
(define key:fs-region "ForcedStrictTypeErrorsInEditRange")
(define key:old-fs-region "OldForcedStrictTypeErrorsInEditRange")
(define key:stime "@timestamp")
(define key:ctime "clientTimestamp")

(define key:curr-stx-region "TypeError1013InEditRange")
(define key:olde-stx-region "OldTypeError1013InEditRange")
(define key:too-complex "TooComplexErrors")

(define key:old-def "OldTypeError1006InEditRange")
(define key:te-def "TypeError1006InEditRange")

(define min-timestamp "Feb 21, 2023 @ 23:59:49.799")
(define max-timestamp "Apr 14, 2023 @ 23:59:59.999")


(define bg-dark-blue (hex-triplet->color% #x2C6B91))
(define bg-lite-blue (hex-triplet->color% #x357C9F))

(define nocheck-color (list-ref tol-palette* 5))
(define nonstrict-color (list-ref tol-palette* 6))
(define strict-color (list-ref tol-palette* 7))

(define (datetime-floor d)
  (datetime (->year d)
            (->month d)
            (->day d)
            0 0 0 0))

(define fzero (lambda () 0))

(define (hash-++ h k n)
  (hash-update h k (lambda (m) (+ n m)) fzero))

(define (hash-++! h k n)
  (hash-update! h k (lambda (m) (+ n m)) fzero))

(define (hash-add1 h k)
  (hash-update h k add1 fzero))

(define (hash-add1! h k)
  (hash-update! h k add1 fzero))

(define (hash-cons! h k v)
  (hash-update! h k (lambda (xx) (cons v xx)) (lambda () '())))

(define (idx->getter title* key)
  (define idx (title-index title* key))
  (lambda (rr) (list-ref rr idx)))

(define (title-index title* str)
  (define idx
    (if (or (string-prefix? str "Studio-Luau-TypeErrors")
            (string=? str key:stime))
      (index-of title* str)
      (let ((suffix (string-append "." str)))
        (index-where title* (lambda (x) (string-suffix? x suffix))))))
  (if idx
    idx
    (raise-arguments-error 'title-index "no match" "str" str "title*" title*)))

(define (mode<? m0 m1)
  (< (mode->int m0) (mode->int m1)))

(define (mode->int m)
  (index-of roblox-mode* m))

(define (posix->iso x)
  (define str (datetime->iso8601 (posix->datetime x)))
  (define y-t* (string-split str "T"))
  (car y-t*)
  #;(define s-ms (string-split (second y-t*) "."))
  #;(string-append (car y-t*) " " (car s-ms)))

(define (snoc x x*)
  (append x* (list x)))

(define (comma-join xx)
  (string-join xx ","))

(define (displaycsv x)
  (displayln (comma-join x)))

(define (saturday? dd)
  (equal? (~t dd "E") "Sat"))

(define (weekend? dd)
  (member (~t dd "E") '("Sat" "Sun")))

(define (string->number* str #:err [err #true])
  (define v (string->number (num-clean str)))
  (if (real? v)
    v
    (and err (raise-argument-error 'string->number* "(stringof real?)" str))))

(define (time-dist-ticks)
  (define (time-dist-layout ax-min ax-max)
    (define d0 (posix->datetime ax-min))
    (define d1 (posix->datetime ax-max))
    (cons
      (pre-tick (->posix d0) #true)
      (let loop ((acc (+days (datetime (->year d0) (->month d0) (->day d0) 0 0 0 0) 1)))
        (cond
          [(datetime<? acc d1)
           (cons (pre-tick (->posix acc) #true)
                 (loop (+days acc 1)))]
          [else
           (list (pre-tick (->posix d1) #true))]))))
  (define (time-dist-format ax-mix ax-max pt*)
    (cons
      (posix->iso (pre-tick-value (first pt*)))
      (snoc
        (posix->iso (pre-tick-value (last pt*)))
        (for/list ((_ (in-list (cddr pt*)))
                   (i (in-naturals)))
          (make-string i)))))
  (ticks time-dist-layout time-dist-format))

(define (percent-ticks ymax)
  (define num-ticks 3)
  (ticks
    (linear-major-layout num-ticks ymax)
    percent-format))

(define (linear-major-y-ticks num-ticks)
  (ticks
    (linear-major-layout num-ticks)
    linear-major-format))

(define ((linear-major-layout num-ticks [ymax #f]) ax-min ax-max)
  (for/list ((ii (in-list (linear-seq ax-min (or ymax ax-max) num-ticks))))
    (pre-tick (exact-floor ii) #true)))

(define (linear-major-format ax-min ax-max pre-ticks)
  (map (compose1 number->string pre-tick-value) pre-ticks))

(define (percent-format ax-min ax-max pre-ticks)
  (map (compose1 (lambda (n) (format "~a%" n)) pre-tick-value) pre-ticks))

(define (num-clean str)
  (let* ((str (string-replace str "," ""))
         (str (unstr str))
         (str (if (string=? str "-") "0" str)))
    str))

(define (unstr str)
  (let* ((L (string-length str))
         (ne (non-empty-string? str))
         (str (if (and ne (eq? #\" (string-ref str (- L 1)))) (substring str 0 (- L 1)) str))
         (str (if (and ne (eq? #\" (string-ref str 0))) (substring str 1) str)))
    str))

(define (ts->datetime str)
  ;; http://unicode.org/reports/tr35/tr35-dates.html#Date_Field_Symbol_Table
  (parse-datetime (unstr str) "MMM d, y @ HH:mm:ss.S"))

(define (type-error-codes)
  (file->value (build-path data-dir "typeerror.rktd")))

(define (intcode->str code* ii)
  (for/first ((kv (in-list code*))
             #:when (eqv? (car kv) ii))
    (cadr kv)))

(define (str->intcode str)
  (define mm (regexp-match #rx"TypeError([0-9][0-9][0-9][0-9])InEdit" str))
  (unless mm
    (raise-argument-error 'str->intcode "TypeErrorNNNN string?" str))
  (string->number (cadr mm)))

(define (str->tename str code#)
  (hash-ref code# (str->intcode str)))

(define (npct a b)
  (* 100 (/ a b)))

(define (pct a b)
  (rnd (npct a b)))

(define (rnd n)
  (~r #:precision '(= 2) n))

(define (save-pict+ fn pp)
  (printf "save-pict: ~a~n" (file-name-from-path fn))
  (save-pict fn pp))

