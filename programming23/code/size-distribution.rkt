#lang racket/base

(provide
  distro-cleanup)

(require
  "base.rkt"
  plot/no-gui
  racket/list
  racket/format
  math/statistics
  racket/file
  racket/path
  (only-in plot/utils ->pen-color linear-seq)
  pict pict-abbrevs
  racket/draw
  (only-in gregor posix->datetime ->posix parse-datetime
           days-between +days))

;; ---

(define (rnd x) (~r #:precision '(= 2) x))

;; for a big figure:
;;  w 700
;;  h 180

(define (mean* n*)
  (if (null? n*) -99
    (for/fold ((tt 0) (bb 0) #:result (/ tt bb))
              ((n (in-list n*)))
      (values (+ tt n) (+ bb 1)))))

(define (median* _ n*)
  (if (null? n*) -99 (list-ref n* (quotient (length n*) 2))))

(define (insert n n*)
  (let loop ((k* n*))
    (if (null? k*)
      (cons n k*)
      (if (< (car k*) n)
        (cons (car k*) (cons n (cdr k*)))
        (cons (car k*) (loop (cdr k*)))))))

(define (f:plot-simple-distro h# summary key out-kind
                              #:x-max [x-max #f] #:y-max [y-max #f])
  (define ww 200)
  (define hh 180)
  (define out-file (build-path img-dir (format "~a-distribution.~a" key out-kind)))
  (define -x* (box '()))
  (define -y* (box '()))
  (define dummy-min 99)
  (define *xmin (box dummy-min))
  (define (cons! z z*) (set-box! z* (cons #;insert z (unbox z*))))
  (define my-bars
    (rectangles
      #:alpha 0.4
      (for*/list (((-kk vv) (in-hash h#))
                   (kk (in-value
                         (begin
                           (set-box! *xmin (min (or -kk dummy-min) (unbox *xmin)))
                           (or -kk -1))))
                   #:when (begin
                            #;(cons! kk -x*)
                            #;(cons! vv -y*)
                            (< 2 vv)))
        (vector (ivl (- kk 1/2) (+ kk 1/2))
                (ivl 2 vv)))))
  (parameterize ([plot-x-far-ticks no-ticks]
                 [plot-y-far-ticks no-ticks]
                 [plot-x-ticks (linear-major-y-ticks 3)]
                 [plot-y-ticks (linear-major-y-ticks 3)]
                 [plot-font-size 18]
                 [plot-font-family 'roman]
                 [rectangle-color bg-dark-blue]
                 [rectangle-line-color bg-dark-blue]
                )
    ;; max median mean stddev
    (define x-med (second summary))
    (printf "summary of ~a :: max ~a median ~a mean ~a stddev ~a~n"
            key (first summary) x-med (exact->inexact (third summary)) (fourth summary))
    (printf "plot-file ~a~n" (path->string (file-name-from-path out-file)))
    (with-handlers ((exn:fail? (lambda (x) (printf "die~n"))))
      (plot-file
        my-bars
        out-file
        out-kind
        #:width ww
        #:height hh
        #:x-min (unbox *xmin)
        #:x-max x-max
        #:y-min 0
        #:y-max y-max
        #:x-label "# Records"
        #:y-label #f #:title #f)))
  (void))

(define (f:plot-codebase-distros x y out-kind limit)
  (f:plot-size-distros x y out-kind #:x-max limit #:y-max limit #:keys '(editrange lines files)))

(define (f:plot-session-distros x y out-kind)
  (define limit 1000)
  (f:plot-size-distros x y out-kind #:x-max limit #:y-max limit #:keys '(timespan event-count)))

(define (f:plot-size-distros x y out-kind #:x-max [x-max #f] #:y-max [y-max #f] #:keys [keys #f] )
  (define h# (file->value x))
  (define s# (file->value y))
  ;; (displayln (hash-keys h#))
  ;; (timespan event-count editrange lines files)
  (for (((kk -vv) (in-hash h#))
        #:when (or (not keys) (memq kk keys)))
    (define vv (distro-cleanup kk -vv))
    (define summary (hash-ref s# kk))
    (f:plot-simple-distro vv summary kk out-kind #:x-max x-max #:y-max y-max))
  (void))

(define (distro-cleanup kk -vv)
  (cond
    [(eq? kk 'editrange)
     (value-cleaning (lambda (n) (< n 4000000000)) -vv)]
    [(eq? kk 'timespan)
     (key-normalize (lambda (k) (quotient k 1000)) -vv)]
    [else
      -vv]))

(define (value-cleaning f h#)
  (for/hash (((k v) (in-hash h#))
             #:when (f v))
    (values k v)))

(define (key-normalize f h#)
  (define m (make-hash))
  (for (((k v) (in-hash h#))
        #:when k)
    (hash-update! m (f k) (lambda (n) (+ v n)) (lambda () 0)))
  m)

(module+ main
  (define out-kind 'pdf) ;; png
  (define fname "size-distributions.rktd")
  (f:plot-codebase-distros
    (build-path data-dir fname)
    (build-path data-dir (string-append "summary-of-" fname))
    out-kind
    1000)
  (f:plot-session-distros
    (build-path data-dir fname)
    (build-path data-dir (string-append "summary-of-" fname))
    out-kind)
  )



