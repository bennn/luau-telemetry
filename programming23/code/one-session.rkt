#lang racket

(require
  "base.rkt"
  pict pict-abbrevs
  plot/no-gui)

;; ---

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
(define (row->mode x) (tenth (cdr x)))

(define mod-te-color 4)
(define mod-fs-color 5)

(define (plot-one-timeline row*)
  (define getx
    (let ((x0 (row->ctime (car row*))))
      (lambda (rr)
        (- (row->ctime rr) x0))))
  (define gety-te row->te-mod)
  (define gety-fs row->fs-mod)
  (define *ymax (box 0))
  (define te-point*
    (points
      (allpoints row* getx gety-te #:ymax *ymax)
      #:color mod-te-color
      #:sym 'plus))
  (define fs-point*
    (points
      (allpoints row* getx gety-fs #:ymax *ymax)
      #:color mod-fs-color
      #:sym 'times))
  (parameterize ([plot-x-far-ticks no-ticks]
                 [plot-y-far-ticks no-ticks]
                 [plot-x-ticks (linear-major-y-ticks 4)]
                 [plot-y-ticks (linear-major-y-ticks 3)]
                 [plot-font-size 10]
                 [plot-font-family 'roman])
    (define ymax (unbox *ymax))
    (plot-pict
      #:title #f
      #:y-label #f
      #:x-label #f
        #;(timespan-label (vector-ref (first point*) 0) (vector-ref (last point*) 0) (length point*))
      #:y-max (+ ymax (if (< ymax 10) 1 10))
      #:width  700
      #:height 180
      #:legend-anchor 'no-legend
      (list
        (modeswitch-vline row* getx ymax)
        (modswitch-vline row* getx ymax)
        te-point*
        fs-point*
        ))))

(define (modeswitch-vline timeline getx y)
  (define prev (row->mode (car timeline)))
  (define yoff 0)
  (define rule+point*
    (for*/list ((row (in-list timeline))
                (mm (in-value (row->mode row)))
                #:when (begin0 (not (equal? mm prev)) (set! prev mm)))
      (define xx (getx row))
      (define y+ (begin0 (+ y (modulo yoff 10)) (set! yoff (+ yoff 3))))
      (list
        (vrule xx #:color 0 #:width 1)
        (point-pict (vector xx y+)
                    (mode-pict mm)
                    #:point-sym 'none
                    #:anchor 'center))))
  (list
    ;; draw all rules first, then points on top
    #;(map car rule+point*)
    (map cadr rule+point*)))

(define (mode-pict str)
  (add-rounded-border
    #:background-color "white"
    #:x-margin 4
    #:y-margin 2
    (ctext (micro-mode str))))

(define (micro-mode str)
  (case str
    ((strict) "s")
    ((nonstrict) "ns")
    ((nocheck) "nc")
    (else (raise-argument-error 'micro-mode "mode?" str))))

(define (ctext str)
  (text str 'modern 14))

(define modswitch-y 10) ;; not too tall

(define (modswitch-vline timeline getx y)
  (for/list ((row (in-list timeline))
             #:when (row->switchedmodule row))
    (define xx (getx row))
    (vrule xx
           ;; 0 modswitch-y
           #:color 0 #:width 1 #:alpha 0.3)))

(define (allpoints tt getx gety #:ymax [ymax #f])
  (define up! (if ymax (lambda (y) (set-box! ymax (max (unbox ymax) y))) values))
  (for/list ((row (in-list tt)))
    (define yy (gety row))
    (up! yy)
    (vector (getx row) yy)))

(define (go-sample row**)
  (for/list ((row* (in-list row**)))
    (plot-one-timeline row*)))

(define (interesting-row* row**)
  (define long-size 20)
  (define long*
    (for/list ((rr (in-list row**))
               #:when (<= long-size (length rr)))
      rr))
  (define N (length long*))
  (printf "num long ~a~n" N)
  (define sort*
    (sort long* > #:key length))
  (if (<= N 6)
    sort*
    (take sort* 20)))

;; ---

(define out-kind 'png)

(module+ main
  (define prefix "error-density-ss")
  (define mode* '(hasup hasdown)) ;; multimod ;;'(nonstrict strict))
  (for ((mm (in-list mode*))
        (pfinal (in-list (list values values)
                         #;(list (lambda (x) (list (list-ref x 6) (list-ref x 18)))
                               (lambda (x) (list (list-ref x 4) (list-ref x 5)))))))
    (define in-file (build-path data-dir (format "~a-~a.rktd" prefix mm)))
    (define out-file (build-path img-dir (format "timeline-~a.~a" mm out-kind)))
    (define vv (file->value in-file))
    (define row** (pfinal (interesting-row* vv)))
    (define pp* (go-sample row**))
    (printf "save pict ~a~n" (path->string (file-name-from-path out-file)))
    (save-pict
      out-file
      (apply vc-append 20 pp*))
    (void))
  (void))


