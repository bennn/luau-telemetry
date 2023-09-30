#lang racket

(require
  "base.rkt"
  plot/no-gui
  )

;; ---

(define agree-color 2)
(define disagree-color 1)
(define neutral-color 4)

(define out-kind 'pdf)

(define (row->te rr) (first rr))
(define (row->fs rr) (second rr))

(define (compass-plot mode)
  (define fname (mode->filename mode))
  (define out-file (build-path img-dir (format "compass-~a.~a" mode out-kind)))
  (define row** (file->value fname))
  (define h# (init#))
  (void
    (for ((row* (in-list row**))
          #:unless (null? (cdr row*)))
      (define curr (first row*))
      ;; TODO !maybe! want old vs curr in range ... instead of overall
      (for/fold ((prev-te (row->te curr))
                 (prev-fs (row->fs curr)))
                ((row (in-list (cdr row*))))
        (define curr-te (row->te row))
        (define curr-fs (row->fs row))
        (define delta-te (- curr-te prev-te))
        (define delta-fs (- curr-fs prev-fs))
        (define key
          (cond
            [(= 0 delta-te delta-fs)
             'zero]
            [(= 0 delta-te)
             'fs-only]
            [(= 0 delta-fs)
             'te-only]
            [(or
               (and (< 0 delta-te) (< 0 delta-fs))
               (and (< delta-te 0) (< delta-fs 0)))
             'agree]
            [(or
               (and (< 0 delta-te) (< delta-fs 0))
               (and (< delta-te 0) (< 0 delta-fs)))
             'disagree]
            [else
              (error 'non-exhaustive)]))
        (hash-add1! h# key)
        (values curr-te curr-fs))))
  (define *ymax (box #f))
  (define my-hist
    (let* ((num-zero (hash-ref h# 'zero))
           (num-te (hash-ref h# 'te-only))
           (num-fs (hash-ref h# 'fs-only))
           (num-disagree (hash-ref h# 'disagree))
           (num-agree (hash-ref h# 'agree))
           (num* (list num-zero num-te num-fs num-disagree num-agree))
           (num-total (apply + num*))
           (_ (set-box! *ymax (apply max (map (lambda (n) (npct n num-total)) num*))))
           )
#;(displayln (vector "te only" (npct num-te num-total)))
      (discrete-histogram
        (list
          (vector "zero" (npct num-zero num-total))
          (vector "te only" (npct num-te num-total))
          (vector "bg only" (npct num-fs num-total))
          (vector "dis." (npct num-disagree num-total))
          (vector "agree" (npct num-agree num-total)))
        #:color neutral-color
        #:line-color neutral-color)))
  (define ymax 50 #;(round-10 (unbox *ymax)))
  (parameterize ([plot-x-far-ticks no-ticks]
                 [plot-y-far-ticks no-ticks]
                 #;[plot-x-ticks (linear-major-y-ticks 4)]
                 [plot-y-ticks (percent-ticks ymax)]
                 [plot-font-size 10]
                 [plot-font-family 'roman])
    (plot-file
      my-hist
      out-file
      out-kind
      #:width  180
      #:height 180
      #:y-max  ymax
      #:y-min 0
      ; #:x-max 150
      ; #:x-min 0
      #:title #f
      #:y-label #f
      #:x-label #f
      ))
  (void))

(define (round-10 n)
  (* 10 (exact-ceiling (/ n 10))))

(define (init#)
  ;; TODO maybe combine all disagrees
  (make-hash '((fs-only  . 0) (te-only . 0) (zero . 0) (agree . 0) (disagree . 0))))

(define (mode->filename mm)
  (build-path data-dir (format "error-density-ss-~a.rktd" mm)))

(define (go mode*)
  (for-each compass-plot mode*))


;; ---

(module+ main
  (go (values #;cddr roblox-mode*))
  (void))

