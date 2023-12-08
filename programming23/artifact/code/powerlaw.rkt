#lang racket

;; Prep data for R to check power law shape using the poweRlaw package

(require
  (only-in "size-distribution.rkt" distro-cleanup)
  "base.rkt")

(module+ test (require rackunit))

;; ---

(define (hist->pdf hh)
  (define N (apply + (hash-values hh)))
  (for/hash (((kk vv) (in-hash hh)))
    (values kk (npct vv N))))

(define (hist->cdf hh)
  (define-values [N nmax]
    (for/fold ((nsum 0)
               (nmax 0))
              ((vv (in-hash-values hh)))
      (values (+ nsum vv) (max vv nmax))))
  (define *pct-so-far (box 0))
  (for/hash ((kk (in-range (+ nmax 1)))
             #:when (hash-has-key? hh kk))
    (define curr-pct (npct (hash-ref hh kk) N))
    (define next-pct (+ (unbox *pct-so-far) curr-pct))
    (set-box! *pct-so-far next-pct)
    (values kk next-pct)))

(define (hash->sorted-list hh)
  (sort (hash->list hh) < #:key car))

(module+ test
  (test-case "pdf cdf"
    (define hh (hash 1 1 2 1 3 1 4 1))
    (check-equal? (hash->sorted-list (hist->pdf hh)) '((1 . 25) (2 . 25) (3 . 25) (4 . 25)))
    (check-equal? (hash->sorted-list (hist->cdf hh)) '((1 . 25) (2 . 50) (3 . 75) (4 . 100)))))

;; ---

(module+ main
  (define fname "size-distributions.rktd")
  (for (((name -hist) (in-hash (file->value (build-path data-dir fname)))))
    (define hist (distro-cleanup name -hist))
    (with-output-to-file
      (format "../out/distributions/~a.csv" name)
      #:exists 'replace
      (lambda ()
        (define-values [N nmax]
          (for/fold ((nsum 0)
                     (nmax 0))
                    ((vv (in-hash-values hist)))
            (values (+ nsum vv) (max vv nmax))))
        (for* ((kk (in-range (+ 1 nmax)))
               (vv (in-value (hash-ref hist kk #f)))
               #:when vv)
          (printf "~a,~a~n" kk vv))
        (void)))
    (void))
  (void))

