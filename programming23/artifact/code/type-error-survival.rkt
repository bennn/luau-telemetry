#lang racket

(require
  math/statistics
  "base.rkt")

;; ---

(define mode-key* roblox-mode*)

(define (count-up-down n*)
  ;; entries = (- curr olde)
  ;; hi = bad = introduce error
  (for/fold ((hi 0)
             (zz 0)
             (lo 0)
             #:result (list hi zz lo))
            ((n (in-list n*)))
    (cond
      [(< 0 n)
       (values (+ hi 1) zz lo)]
      [(= 0 n)
       (values hi (+ zz 1) lo)]
      [else
       (values hi zz (+ lo 1))])))

(define (go)
  (define h#*
    (for/list ((kk (in-list mode-key*)))
      (file->value (build-path data-dir (format "type-error-survival-ss-~a.rktd" kk)))))
  (define key*
    (let* ((k# (for*/fold ((acc (hash)))
                          ((h# (in-list h#*))
                           (k (in-hash-keys h#)))
                 (hash-set acc k #true)))
           (k* (hash-keys k#))
           (k* (sort k* <)))
      k*))
  (define tecode* (type-error-codes))
  (define row*
    (for/list ((kk (in-list key*)))
      (cons (intcode->str tecode* kk)
            (for/list ((h# (in-list h#*)))
              (count-up-down (hash-ref h# kk '()))))))
  (tex-table row*))

(define (tex-table row*)
  (printf "\\begin{tabular}{lr@{~~}r@{~~}rr@{~~}r@{~~}rr@{~~}r@{~~}r}~n")
  (printf "  & \\zerowidth{\\mnocheck{}} & & & \\zerowidth{\\mnonstrict{}} & & & \\zerowidth{\\mstrict{}} & & \\\\~n")
  (printf "  & \\rcell{Add} & \\ycell{Keep} & \\gcell{Drop} & \\rcell{Add} & \\ycell{Keep} & \\gcell{Drop} & \\rcell{Add} & \\ycell{Keep} & \\gcell{Drop} \\\\\\hline~n")
  (for ((row (in-list row*)))
    (printf "  ~a " (first row))
    (for ((triple (in-list (cdr row))))
      (apply printf "& {~a} & {~a} & {~a} " triple))
    (printf "\\\\~n"))
  (printf "\\end{tabular}~n")
  (void))

(module+ main (go))

