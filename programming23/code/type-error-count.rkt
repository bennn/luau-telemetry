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

(define (go2 mode)
  (printf "go2 \\m~a{}~n" mode)
  (define fname (build-path data-dir (format "te-editrange-ss-~a.rktd" mode)))
  (define all-err# (file->value fname))
  (define code* (type-error-codes))
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
  (define pct* (for/list ((kv (in-list ranked))
                          #:when (< 0 (cdr kv)))
                 (list (car kv) (pct (cdr kv) total-err))))
  (tex-table pct*))

(define (pct a b)
  (~r #:precision '(= 2) (* 100 (/ a b))))

(define (tex-table pct*)
  (printf "\\begin{tabular}{lr}~n")
  (for ((pct (in-list pct*)))
    (printf "  \\code{~a} & \\pct{~a} \\\\~n" (first pct) (second pct)))
  (printf "\\end{tabular}~n")
  (void))

(define (go* mode*)
  (for-each go2 mode*))

(module+ main
  #;(go "overview.txt")
  #;(go "modswitch-overview.txt")
  (go* roblox-mode*)
  (void))

