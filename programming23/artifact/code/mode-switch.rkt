#lang racket

(require
  "base.rkt"
  math/statistics
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

(define (go in-file*)
  (define-values [all-up* all-down*]
    (for*/fold ((up* '()) (down* '()))
               ((fn (in-list in-file*))
                (vv (in-value (file->value fn)))
                (row* (in-list vv))
                #:when (and (not (null? row*)) (not (null? (cdr row*)))))
      (let loop ((prev (car row*))
                 (curr* (cdr row*))
                 (up* up*)
                 (down* down*))
        (cond
          [(null? curr*)
           (values up* down*)]
          [(is-upgrade? (row->mode prev) (car curr*))
           (loop (car curr*) (cdr curr*)
                 (cons (- (row->te-mod (car curr*)) (row->te-mod prev))
                       up*)
                 down*)]
          [(is-downgrade? (row->mode prev) (car curr*))
           (loop (car curr*) (cdr curr*)
                 up*
                 (cons (- (row->te-mod (car curr*)) (row->te-mod prev))
                       down*))]
          [else
           (loop (car curr*) (cdr curr*)
                 up* down*)]))))
  (printf "wow ~a up, ~a down~n" (length all-up*) (length all-down*))
  (printf "=== UP~n")
  (te-stats all-up*)
  (printf "=== DOWN~n")
  (te-stats all-down*)
  (void))

(define (is-upgrade? prev-mode curr-row)
  (and (not (row->switchedmodule curr-row))
       (mode<? prev-mode (row->mode curr-row))))

(define (is-downgrade? prev-mode curr-row)
  (and (not (row->switchedmodule curr-row))
       (mode<? (row->mode curr-row) prev-mode)))

(define (mode<? m0 m1)
  (< (mode->int m0) (mode->int m1)))

(define (mode->int mm)
  (case mm
    ((nocheck) 0)
    ((nonstrict) 1)
    ((strict) 2)
    (else (error 'die))))

(define (te-stats te*)
  (define mm (mean te*))
  (printf "min ~a max ~a median ~a mean ~a stddev ~a~n"
          (apply min te*)
          (apply max te*)
          (median < te*)
          (~r #:precision '(= 2) mm)
          (stddev/mean mm te*))
  (void))


;; ---

(define out-kind 'png)

(module+ main
  (define prefix "error-density-ss")
  (define mode* '(hasup hasdown))
  (define in-file*
    (for/list ((mm (in-list mode*)))
      (build-path data-dir (format "~a-~a.rktd" prefix mm))))
  (go in-file*)
  (void))

;; wow 619 up-pairs, 583 down-pairs
;; === UP
;; min -3 max 57 median 0 mean 2.62 stddev 6.8390518952707335
;; === DOWN
;; min -48 max 30 median 0 mean -0.31 stddev 4.498748355271854


