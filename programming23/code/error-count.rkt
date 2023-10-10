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
  (printf "TE~n") (go-counts row->te mode)
  (printf "FS~n") (go-counts row->fs mode)
  (printf "TE mod~n") (go-counts row->te-mod mode)
  (printf "FS mod~n") (go-counts row->fs-mod mode)
  (void))

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
  (printf "  ~a & ~a & (\\pct{~a}) & ~a & [\\pct{~a}] & ~a & [\\pct{~a}] \\\\~n"
          mode num-up pct-up
               num-same pct-same
               num-down pct-down)
  #;(printf "~a ~a :: ~a ++ , ~a ==, ~a --~n"
          (object-name getter) mode num-up num-same num-down)
  (void))

(define ((row->X-density row->X) rr)
  (define N (row->lines rr))
  (if (zero? N)
    #;(raise-arguments-error 'row->te-density "zero lines what to do" "row" rr)
    #f
    (/ (row->X rr) N)))

(define row->te-density (row->X-density row->te))
(define row->fs-density (row->X-density row->fs))

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

;(define (row->X-density-diff row->X)
;  (let ((prev (box #f)))
;    (lambda (rr)
;      (if (eq? rr 'reset)
;        (begin (set-box! prev #f) #f)
;        (let ()
;          (define olde (unbox prev))
;          (define curr (row->X rr))
;          (set-box! prev curr)
;          (if (and olde curr)
;            (- curr olde)
;            #f))))))
;
;(define (row->te-density-diff rr) ((row->X-density-diff row->te-density) rr))
;(define (row->fs-density-diff rr) ((row->X-density-diff row->fs-density) rr))

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

(define row->fs-density-diff
  (let ((prev (box #f)))
    (lambda (rr)
      (if (eq? rr 'reset)
        (begin (set-box! prev #f) #f)
        (let ()
          (define olde (unbox prev))
          (define curr (row->fs-density rr))
          (set-box! prev curr)
          (if (and olde curr)
            (- curr olde)
            #f))))))

(define (plot-error-density-diff mode)
  (row->te-density-diff 'reset)
  (one-line-plot mode row->te-density-diff))

(define (plot-fs-density-diff mode #:y-max [y-max #f])
  (row->te-density-diff 'reset)
  (one-line-plot mode row->fs-density-diff #:point 'times #:y-max y-max))

(define (one-line-plot mode row->y #:point [point-sym 'plus] #:y-max [-y-max #f])
  (define row** (file->value (build-path data-dir (format "error-density-ss-~a.rktd" mode))))
  (define row->x row->ctime)
  (define *ymax (box 0))
  (define *ymin (box 0))
  (define *xmax (box 0))
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
            (and x y
                 (set-box! *ymax (max (unbox *ymax) y))
                 (set-box! *ymin (min (unbox *ymin) y))
                 (set-box! *xmax (max (unbox *xmax) x))
                 (vector x y))))
        #:size 10 #:color ii #:alpha 0.5 #:sym point-sym)))
  (define y-max (or -y-max 50))
  (parameterize ([plot-x-far-ticks no-ticks]
                 [plot-y-far-ticks no-ticks]
                 [plot-x-ticks (linear-major-y-ticks 4)]
                 [plot-y-ticks (linear-major-y-ticks 3)]
                 [plot-font-size 18]
                 [plot-font-family 'roman])
    (define out-file (build-path img-dir (string-replace (format "error-count-~a-~a.~a" mode (object-name row->y) out-kind) ">" "-")))
    (printf "plot-file ~a~n" (path->string (file-name-from-path out-file)))
    (printf "ymax ~a ymin ~a xmax ~a~n"
            (unbox *ymax) (unbox *ymin) (unbox *xmax))
    (define pp
      (plot-pict
        (list
          my-lines
          (hrule 0 #:alpha 0.5 #:color 0))
        #:width  700
        #:height 120
        #:y-max (+ y-max)
        #:y-min (- y-max)
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
  #;(plot-error-diff mode)
  (plot-error-density-diff mode)
  #;(plot-fs-density-diff mode)
  (void))

;; ---

(module+ main
  (printf "error-count, density~n")
  #;(for-each simple-go roblox-mode*)
  (for-each plot-go (values #;cddr roblox-mode*))
  (void))


