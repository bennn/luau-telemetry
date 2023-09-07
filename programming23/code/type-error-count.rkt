#lang racket/base

;; Q. Rank most to least common

(require
  "base.rkt"
  plot/no-gui
  racket/list
  racket/port
  racket/string
  racket/format
  math/statistics
  racket/file
  racket/path
  (only-in plot/utils ->pen-color linear-seq)
  pict pict-abbrevs
  racket/draw)

;; ---

(define (str->intcode str)
  (define mm (regexp-match #rx"TypeError([0-9][0-9][0-9][0-9])InEdit" str))
  (unless mm
    (raise-argument-error 'str->intcode "TypeErrorNNNN string?" str))
  (string->number (cadr mm)))

(define (str->tename str code#)
  (hash-ref code# (str->intcode str)))

(define (overview->te# fn)
  (define ln* (file->lines fn))
  (define h-idx (index-where ln* (lambda (str) (string=? str "#hash(" ))))
  (define h-ln* (drop ln* h-idx))
  (with-input-from-string (string-join h-ln*)
    read))

(define (go fname)
  (define code* (type-error-codes))
  (define all-err# (overview->te# (build-path data-dir fname)))
  (define h#
    (for/hash ((nnnn (in-list code*)))
      (define num (first nnnn))
      (define name (second nnnn))
      (define num-te
        (for/sum (((str count) (in-hash all-err#))
                  #:when (string-contains? str "TypeError"))
          (if (eqv? num (str->intcode str))
            count
            0)))
      (values name num-te)))
  (define total-err (apply + (hash-values h#)))
  (define ranked (sort (hash->list h#) > #:key cdr))
  (define pct* (for/list ((kv (in-list ranked)))
                 (list (car kv) (pct (cdr kv) total-err))))
  (tex-table pct*))

(define (pct a b)
  (~r #:precision '(= 2) (* 100 (/ a b))))

(define (tex-table pct*)
  (printf "\\begin{tabular}{lr}~n")
  (for ((pct (in-list pct*)))
    (printf "  \\code{~a} & ~a\\% \\\\~n" (first pct) (second pct)))
  (printf "\\end{tabular}~n")
  (void))

(module+ main
  #;(go "overview.txt")
  (go "modswitch-overview.txt")
  (void))

