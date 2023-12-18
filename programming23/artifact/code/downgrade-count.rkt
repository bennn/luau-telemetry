#lang racket

(require
  "base.rkt")

(define row->modswitch ninth)
(define row->mode last)

(define (go fn what)
  (define vv (file->value fn))
  (define total-session (length vv))
  (define down-from# (make-hash (map (lambda (x) (cons x 0)) roblox-mode*)))
  (define mode-compare
    (if (equal? what "downgrade")
      mode<?
      (lambda (next curr) (mode<? curr next))))
  (printf "~a total sessions~n" total-session)
  (void
    (for ((ss (in-list vv)))
      (define down#
        (let loop ((row* ss) (acc (hash)))
          (define curr-mode (row->mode (car row*)))
          (define next-mode (row->mode (cadr row*)))
          (define next-switch (row->modswitch (cadr row*)))
          (define change? (and (not next-switch) (mode-compare next-mode curr-mode)))
          (define acc+
            (if change?
              #;(hash-set acc curr-mode #true)
              (hash-add1 acc (list curr-mode next-mode))
              acc))
          (if (null? (cddr row*))
            acc+
            (loop (cdr row*) acc+))))
      (for ((mm (in-list (hash-keys down#))))
        (hash-add1! down-from# mm))
      #;(for (((k v) (in-hash down#)))
        (hash-++! down-from# k v))
    ))
  (define NN (apply + (hash-values down-from#)))
  (printf "~a with ~a~n" NN what)
  (pretty-write down-from#)
  (for (((k v) (in-hash down-from#)))
    (printf " ~a : ~a\\%~n" k (pct v NN)))
  (void))

(define (count-modswitch fn)
  ;; TODO
  (void))

(module+ main
  (with-output-to-file (build-path data-dir "downgrade-count.txt")
    #:exists 'replace
    (lambda ()
      (let ((what "upgrade"))
        (displayln what)
        (go (build-path data-dir "error-density-ss-hasup.rktd") what))
      (let ((what "downgrade"))
        (displayln what)
        (go (build-path data-dir "error-density-ss-hasdown.rktd") what))
      #;(newline)
      #;(count-modswitch (build-path data-dir "error-density-ss-multimod.rktd"))
      (void))))



