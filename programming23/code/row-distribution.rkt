#lang racket/base

(require
  "base.rkt"
  plot/no-gui
  racket/file
  (only-in plot/utils ->pen-color linear-seq)
  pict pict-abbrevs
  racket/draw
  (only-in gregor posix->datetime ->posix parse-datetime
           days-between +days))

;; ---

(define (f:plot-clienttime-distro data-file -out-kind)
  (define hh (file->value data-file))
  (parameterize ([plot-x-far-ticks no-ticks]
                 [plot-y-far-ticks no-ticks]
                 [plot-x-ticks (time-dist-ticks)]
                 [plot-y-ticks (linear-major-y-ticks 3)]
                 [plot-font-size 11]
                 [plot-font-family 'roman]
                 [rectangle-color bg-dark-blue]
                 [rectangle-line-color bg-dark-blue]
                )
    (define xmin #f)
    (define xmax #f)
    (define my-bars
      (rectangles
        (for/list ((f+s (in-list hh)))
          (define log-sec (car f+s))
          (when (or (not xmin) (< log-sec xmin))
            (set! xmin log-sec))
          (when (or (not xmax) (< xmax log-sec))
            (set! xmax log-sec))
          (define num-row (cdr f+s))
          (vector (ivl log-sec (+ log-sec 1800))
                  (ivl 2 num-row)))))
    (void
      (set! xmin (datetime-floor (posix->datetime xmin)))
      (set! xmax (+days (datetime-floor (posix->datetime xmax)) 1)))
    (define ymin 0)
    (define ymax (apply max (map cdr hh)))
    (define weekend-color "black")
    (define weekend-shade
      (rectangles
        (for*/list ((ii (in-range (days-between xmin xmax)))
                    (dd (in-value (+days xmin ii)))
                    #:when (saturday? dd))
          (vector (ivl (->posix dd) (->posix (+days dd 2)))
                  (ivl ymin ymax)))
        #:color weekend-color
        #:line-style 'transparent
        #:line-color weekend-color
        #:alpha 0.1))
    (define day-rules
      (for*/list ((ii (in-range (days-between xmin xmax)))
                  (dd (in-value (+days xmin ii))))
        (vrule (->posix dd) #:color "black" #:width 0.5 #:alpha 0.4)))
    (define-values [out-file out-kind]
      (let ((out-kind -out-kind))
        (values (build-path img-dir (format "row-distribution.~a" out-kind)) out-kind)))
    (plot-file
      (list weekend-shade day-rules my-bars)
      out-file
      out-kind
      #:width  700
      #:height 180
      #:x-min (->posix (ts->datetime max-timestamp))
      #:x-max (->posix (ts->datetime min-timestamp))
      #:y-max ymax
      #:y-min ymin
      #:x-label #f #:y-label #f #:title #f)
    (printf "plot-file ~a~n" out-file)
    (void)))

(module+ main
  (f:plot-clienttime-distro
    (build-path data-dir "row-distribution.rktd")
    'pdf ;; 'png
    ))

