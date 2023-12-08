#lang racket/base

(require
  "base.rkt"
racket/pretty
racket/file
(only-in pict-abbrevs max*)
math/statistics)

(define (up h)
  (define vv (for/list ((v (in-hash-keys h)) #:when (and v (< v 4000000000))) v))
  (define mm (mean vv))
  (list* (max* vv) (median < vv) mm (stddev/mean mm vv) (percentile* vv)))

(define (percentile* vv)
  (for/list ((pct '(0.95 0.96 0.97 0.98 0.99)))
    (list pct (quantile pct < vv))))

(define h0 (file->value (build-path data-dir "size-distributions.rktd")))
(define h1
(for/hash ((( kk vv) (in-hash h0)))
  (values kk (up vv))))

(pretty-print-columns 400)
(with-output-to-file (build-path data-dir "summary-of-size-distributions.rktd") #:exists 'replace (lambda () (pretty-write h1)))

