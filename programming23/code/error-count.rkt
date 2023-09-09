#lang racket

(require
  "base.rkt"
  plot/no-gui
  pict pict-abbrevs
  (except-in plot/utils max* min*))

;; ---

(define out-kind 'pdf)

(define (row->te x) (first x))
(define (row->fs x) (second x))
(define (row->te-mod x) (third x))
(define (row->fs-mod x) (fourth x))
(define (row->te-edit x) (fifth x))
(define (row->fs-edit x) (sixth x))
(define (row->lines x) (seventh x))
(define (row->edits x) (eighth x))
(define (row->switchedmodule x) (ninth x))
(define (row->ctime x) (tenth x))

(define (simple-go mode)
  (go-counts row->te mode))

(define (go-counts getter mode)
  (define row** (file->value (build-path data-dir (format "error-density-ss-~a.rktd" mode))))
  (define num-up 0)
  (define num-down 0)
  (define num-same 0)
  (void
    (for ((row* (in-list row**)))
      (for/fold ((prev (getter (car row*))))
                ((row (in-list (cdr row*))))
        (define curr (getter row))
        (cond
          [(< curr prev) (set! num-down (+ 1 num-down))]
          [(> curr prev) (set! num-up (+ 1 num-up))]
          [(and (not (zero? curr))
                (eqv? curr prev)) (set! num-same (+ 1 num-same))]
          [else (void)])
        curr)))
  (define-values [pct-up pct-same pct-down]
    (let ((bot (+ num-up num-down num-same)))
      (apply values (map (lambda (n) (pct n bot)) (list num-up num-same num-down)))))
  (printf "  ~a & ~a & (\\pct{~a}) & ~a & (\\pct{~a}) & ~a & (\\pct{~a}) \\\\~n"
          mode num-up pct-up
               num-same pct-same
               num-down pct-down)
  #;(printf "~a ~a :: ~a ++ , ~a ==, ~a --~n"
          (object-name getter) mode num-up num-same num-down)
  (void))

(define (row->te-density rr)
  (define N (row->lines rr))
  (if (zero? N)
    #;(raise-arguments-error 'row->te-density "zero lines what to do" "row" rr)
    #f
    (/ (row->te-mod rr) N)))

(define (plot-error-count mode)
  (one-line-plot mode row->te))

(define row->te-diff
  (let ((prev (box #f)))
    (lambda (rr)
      (if (eq? rr 'reset)
        (begin (set-box! prev #f) #f)
        (let ()
          (define olde (unbox prev))
          (define curr (row->te rr))
          (set-box! prev curr)
          (if olde
            (- curr olde)
            #f))))))

(define (plot-error-diff mode)
  (row->te-diff 'reset)
  (one-line-plot mode row->te-diff))

(define (plot-error-density mode)
  (one-line-plot mode row->te-density))

(define row->te-density-diff
  (let ((prev (box #f)))
    (lambda (rr)
      (if (eq? rr 'reset)
        (begin (set-box! prev #f) #f)
        (let ()
          (define olde (unbox prev))
          (define curr (row->te-density rr))
          (set-box! prev curr)
          (if (and olde curr)
            (- curr olde)
            #f))))))

(define (plot-error-density-diff mode)
  (row->te-density-diff 'reset)
  (one-line-plot mode row->te-density-diff))

(define (one-line-plot mode row->y)
  (define row** (file->value (build-path data-dir (format "error-density-ss-~a.rktd" mode))))
  (define row->x row->ctime)
  (define my-lines
    (for/list ((row* (in-list row**))
               (ii (in-naturals)))
      (row->te-diff 'reset)
      (define x0 (row->ctime (car row*)))
      (define (row->x rr) ;; seconds
        (/ (- (row->ctime rr) x0) 1000))
      (points
        (filter values
          (for/list ((rr (in-list row*)))
            (define x (row->x rr))
            (define y (row->y rr))
            (and x y (vector x y))))
        #:size 10 #:color ii #:alpha 0.5 #:sym 'plus)))
  (parameterize ([plot-x-far-ticks no-ticks]
                 [plot-y-far-ticks no-ticks]
                 [plot-x-ticks (linear-major-y-ticks 4)]
                 [plot-y-ticks (linear-major-y-ticks 3)]
                 [plot-font-size 10]
                 [plot-font-family 'roman])
    (define out-file (build-path img-dir (format "error-count-~a-~a.~a" mode (object-name row->y) out-kind)))
    (printf "plot-file ~a~n" (path->string (file-name-from-path out-file)))
    (define pp
      (plot-pict
        (list
          my-lines
          (hrule 0 #:alpha 0.5 #:color 0))
        #:width  700
        #:height 180
        #:y-max  50
        #:y-min -50
        #:x-max 150
        #:x-min 0
        #:title #f
        #:y-label #f
        #:x-label #f
        ))
    (save-pict out-file (freeze pp))
    (void)))

;; TODO density, counts
(define (plot-go mode)
  #;(plot-error-count mode)
  #;(plot-error-density mode)
  (plot-error-diff mode)
  (plot-error-density-diff mode)
  (void))

;; ---

(module+ main
  (printf "error-count, density~n")
  #;(for-each simple-go roblox-mode*)
  ;; TODO try smaller bounds
  (for-each plot-go (values #;cddr roblox-mode*))
  (void))


