#lang racket

(require
  "base.rkt"
  pict
  pict-abbrevs
  plot/no-gui)

(define te-data*
  '(595137 ;; total
    176309 ;; nocheck
    362964 ;; nonstrict
    22771  ;; strict
    ))

(define fs-data*
  '( 72235735 ;; total
     63784284 ;; nocheck
      7546175 ;; nonstrict
       114259 ;; strict
       ))

(define (mini-bars x-data*)
  (parameterize ([plot-x-far-ticks no-ticks]
                 [plot-y-far-ticks no-ticks]
                 [plot-x-ticks no-ticks]
                 [plot-y-ticks no-ticks])
    (define NN (car x-data*))
    (define FONT 'modern) ;; '(bold . modern)
    (plot-pict
      (for/list ((mm (in-list (cdr x-data*)))
                 (cc (in-list (list nocheck-color nonstrict-color strict-color)))
                 (yy (in-naturals)))
        (define x (* 100 (/ mm NN)))
        (define y (* -2 yy))
        (list
          (point-pict
            (vector x y)
            (text (format "~a%" (rnd x)) FONT 20)
            #:point-sym 'none)
          (rectangles (list (vector
                              (ivl 0 x)
                              (ivl (+ y 1/2) (- y 1/2))))
                      #:alpha 0.7
                      #:color cc
                      #:line-color cc)))
      #:width 180
      #:height 120
      #:y-max 1
      #:y-min -5
      #:x-min 0
      #:x-max 180
      #:title #f
      #:x-label #f
      #:y-label #f)))

(define out-kind 'png)

(module+ main
  (define title* '("Type error" "Background error"))
  (define data* (list te-data* fs-data*))
  (define tag* '(te fs))
  (for ((tt (in-list tag*))
        (dd (in-list data*)))
    (save-pict+
      (build-path img-dir (format "error-by-mode-~a.~a" tt out-kind))
      (mini-bars dd)
      #;(add-rectangle-background
        (mini-bars dd)
        #:x-margin 8
        #:y-margin 8))))

