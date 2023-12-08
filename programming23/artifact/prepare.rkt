#lang racket

;; TODO
;; - [X] error lines, set min session lench (already had that! want max too .. still not too helpful)
;; - [X] put nonstrict typeError + forcedStrict on same plot, are you fixing FS despite not seeing?
;; - [X] new plot
;; - [X] err delta try without lines
;; - [X] err delta, where is the density? mostly add or mostly drop?
;;       ... this is just a count
;; - [X] how is there ever any orange???! are TE = FS? in what cases FS > TE??
;;   - [X] TODO still plenty of orange, print cases!
;; - [X] TE down? plot, etc, need to ignore 0, REBUILD
;;   - lots of mismatches, yikes!
;;   - down? => lots of #f at 1st point, then gone!!
;;   - beyond 1st, usually even or "down" wins => removing errors
;; - [X] plot error density: TE / line or TE / char
;;       min, max, mode / session, 
;; - [X] y/n equal to best point on session? equal to "first"?
;; - [X] try "vector" plot, arrow for [(te0,fs0),(te1,fs1)]
;; - [X] count vertical vs diag, vert = dm, hiw often dm dominate
;; - [X] "compass" diagram for TE vs FS
;;   - how to interpret compass for TE FS?
;;   - anything else we can run correlations on??!
;;   - or a simple barchart
;;   - get 3x for each thanks to modes
;; - [X] how often module switch with many errors?
;; - [X] data cleanting: negative edit range ... wait, none exist??!
;;   !!! editsize-wrong? , over 4M
;;   !!! keep the row but do not use the edit range!!!
;; - [X] do people tend to clean up? hammer a narrative
;;   - find average gap, ignore session if end if within Nx of that gap
;;     (2s average, cutoff after 3s = ignore)
;;    - yes end < start type errors, about 2x of the time
;; - [X] how often errors outside region change? action-at-distance
;;   - cant answer directly, don't have old-overall
;;   - substitute: edit region vs module
;;     - 59,256 total = 33,917 nocheck, 23,606 nonstrict, 1,733 strict
;;     - out of 1,504,780 = 1,341,392 nocheck + 156,883 nonstrict + 6,505 strict
;; ===
;; - [ ] ppl often switch to errors, do they often switch away too?
;;       1. find switch, count #err 2. look ahead to next switch, compare
;; - [ ] count sessions for those bars plots, what's the total they all add to?
;; - [X] for up/down use ONE session get olde/curr counts from that
;;   TE < 1st point
;; - [ ] double-check up/down nonstrict
;; - [ ] build up/down nocheck ... in same ballpark as nonstrict, uncorrelated?
;; - [ ] try regular up/down R2 correlation
;; ===
;; - [ ] switch correlated with good state? (uh what is good?)
;; - [X] set a bound on min/max errors, do not zoom/cutoff
;; - [X] what's happening when up/down mismatch, especially in nonstrict?? te down?
;; - [X] summary plots in paper
;; - [-] ???  dm impact vs everything else
;; - [X] update paper with overview, counts, basic plots!!! ctc analysis

;; - [-] Q no april data, 1-14 is pretty much empty

(require
;  data-frame
  text-table
  plot/no-gui
  (only-in plot/utils ->pen-color linear-seq)
  (only-in gtp-util rnd)
  file/glob
  pict pict-abbrevs
  racket/draw
  racket/runtime-path
  (only-in gregor datetime<? datetime<=? datetime->iso8601 posix->datetime ->posix parse-datetime
           days-between ~t
           minutes-between milliseconds-between current-timezone +days datetime
           ->year ->month ->day ->hours)
  (only-in math/statistics median mean correlation stddev/mean))

;; ---

(define-runtime-path data-dir "data")
(define-logger luau)

(current-timezone "America/Los_Angeles") ;; does this even help? times seem from the data either way :/

(define roblox-mode* '(nocheck nonstrict strict))

(define type-error#
  (for/hash ((kv (in-list (file->value "type-error.rktd"))))
    (values (car kv) (cadr kv))))

(define bg-dark-blue (hex-triplet->color% #x2C6B91))
(define bg-lite-blue (hex-triplet->color% #x357C9F))

(define key:switchedmodule "SwitchedModule")
(define key:mode "Mode")
(define key:sessionid "SessionId")
(define key:files "Files")
(define key:lines "Lines")
(define key:edits "LinesInEditRegion")
(define key:te "TypeErrors")
(define key:fs "ForcedStrictTypeErrors")
(define key:fs-mod "ForcedStrictTypeErrorsInModule")
(define key:old-fs-mod "OldForcedStrictTypeErrorsInModule")
(define key:old-te-mod "OldTypeErrorsInModule")
(define key:te-mod "TypeErrorsInModule")
(define key:te-region "TypeErrorsInEditRegion")
(define key:old-te-region "OldTypeErrorsInEditRegion")
(define key:fs-region "ForcedStrictTypeErrorsInEditRegion")
(define key:old-fs-region "OldForcedStrictTypeErrorsInEditRegion")
(define key:stime "@timestamp")
(define key:ctime "clientTimestamp")

(define key:curr-stx-region "TypeError1013InEditRegion")
(define key:olde-stx-region "OldTypeError1013InEditRegion")
(define key:too-complex "TooComplexErrors")

(define key:old-def "OldTypeError1006InEditRegion")
(define key:te-def "TypeError1006InEditRegion")

(define min-timestamp "Feb 21, 2023 @ 23:59:49.799")
(define max-timestamp "Apr 14, 2023 @ 23:59:59.999")

;; ---

(define (filter/ts-bounds dd)
  (define title* (first dd))
  (define row* (second dd))
  (define min-t (ts->datetime min-timestamp))
  (define max-t (ts->datetime max-timestamp))
  (define row->timestamp (row->datetime (title-index title* key:ctime)))
  (list
    title*
    (for/list ((rr (in-list row*))
               #:when (let ((ts (row->timestamp rr)))
                        (and (datetime<=? min-t ts)
                             (datetime<=? ts max-t))))
      rr)))

(define (filter/modswitch dd)
  (define title* (first dd))
  (define row* (second dd))
  (define row->modswitch (compose1 string->bool (idx->getter title* key:switchedmodule)))
  (list title* (filter row->modswitch row*)))

(define (ctc-info dd)
  (define title* (first dd))
  (define row* (second dd))
  (define row->ctc (compose1 string->number* (idx->getter title* key:too-complex)))
  (define row->session (idx->getter title* key:sessionid))
  (define row->ctime (row->datetime (title-index title* key:ctime)))
  (define render (ctc-render title*))
  (for ((rr (in-list row*)))
    (define ctc (row->ctc rr))
    (when (< 0 ctc)
      (displayln (render rr)))))

(define (explore csv*)
  (define title** (map (lambda (x) (list x (csv->title* x))) csv*))
  (define title*** (group-by cadr title**))
  (unless (null? (cdr title***))
    (printf "WARNING: title mismatch~n")
    ;; (let ((all (apply set-union (map cadar title***))))
    ;;   (with-output-to-file "BTITLE" #:exists 'replace (lambda () (for-each displayln all))))
    (define common (apply set-intersect (map cadar title***)))
    (printf "~n")
    (for ((title** (in-list title***)))
      (printf " ~s~n" (set-subtract (cadar title**) common))
      (for ((fn (in-list (map car title**))))
        (printf "  ~s~n" fn))
      (printf "~n")
      (void))
    (void))
  (printf "example title:~n ~s~n" (cadr (caar title***)))
  (void))

(define (pretty-sample kv# sorted)
  (for ((k (in-list sorted)))
    (define v* (hash-ref kv# k))
    (printf "~a~n ~a~n" k (format-vals v*))))

(define (format-vals v*)
  (values ;; truncate ;; ???
    (if (string->number* (first v*) #:err #f)
      (sort (map string->number* v*) >)
      (shuffle v*))))

(define (t:dataset-overview dd tt)
  (t:dataset-overview* (list (list dd tt))))

(define (t:dataset-overview* csv*)
  (define dd# (make-hash))
  (define tt# (make-hash))
  (void
    (for ((csv (in-list csv*)))
      (build-dataset-overview csv dd# tt#)))
  (let () ;; row counts
    (let* ((n-nocheck (hash-ref dd# 'nocheck 0))
           (n-nonstrict (hash-ref dd# 'nonstrict 0))
           (n-strict (hash-ref dd# 'strict 0)))
      (printf " ~a total logs = ~a nocheck + ~a nonstrict + ~a strict~n"
              (+ n-nocheck n-nonstrict n-strict) n-nocheck n-nonstrict n-strict))
    (let* ((n-modswitch (hash-ref dd# key:switchedmodule)))
      (printf "  ~a due to module switch~n" n-modswitch))
    (printf " ~a total forced strict type errors, ~a in module,~n curr ~a in edit regions,~n olde ~a in edit regions~n"
            (hash-ref dd# key:fs) (hash-ref dd# key:fs-mod)
            (hash-ref dd# key:fs-region)
            (hash-ref dd# key:old-fs-region))
    (define te-region (hash-ref dd# key:te-region))
    (define te-stx-region (hash-ref dd# key:curr-stx-region))
    (define olde-te-region (hash-ref dd# key:old-te-region))
    (define olde-te-stx-region (hash-ref dd# key:olde-stx-region))
    (printf " ~a total type errors, ~a in module,~n curr ~a non-stx + ~a syntax in edit regions,~n olde ~a non-stx + ~a syntax in edit regions~n"
            (hash-ref dd# key:te) (hash-ref dd# key:te-mod)
            (- te-region te-stx-region) te-stx-region
            (- olde-te-region olde-te-stx-region) olde-te-stx-region)
    (void))
  (let* ( ;; timeline counts
         (nsession (hash-ref tt# 'nsession 0))
         (n-nocheck (hash-ref tt# 'nocheck 0))
         (n-nonstrict (hash-ref tt# 'nonstrict 0))
         (n-strict (hash-ref tt# 'strict 0))
         (n-multi (hash-ref tt# 'num-multi 0))
         (n-up (hash-ref tt# 'n-up 0))
         (n-down (hash-ref tt# 'n-down 0)))
    (printf " ~a sessions~n" nsession)
    (printf "  ~a single mode = ~a nocheck + ~a nonstrict + ~a strict~n"
            (+ n-nocheck n-nonstrict n-strict) n-nocheck n-nonstrict n-strict)
    (printf "  ~a multi mode projects~n" n-multi)
    (printf "  ~a mode upgrades~n" n-up)
    (printf "  ~a mode downgrades~n" n-down)
    (void))
 (void))

(define (build-dataset-overview csv dd# tt#)
  (log-luau-info "build overview ...")
  (define-values [dd tt] (if (path-string? csv) (csv->dd-tt csv) (apply values csv)))
  (define title* (first dd))
  (define row* (second dd))
  (define timeline* (cdr tt))
  (define row->mode (compose1 string->symbol (idx->getter title* key:mode)))
  (define row->switch (compose1 string->symbol (idx->getter title* key:switchedmodule)))
  (define row->te (compose1 string->number* (idx->getter title* key:te)))
  (define row->fs (compose1 string->number* (idx->getter title* key:fs)))
  (define row->te-mod
    (let ((m-new (compose1 string->number* (idx->getter title* key:te-mod))))
      (lambda (rr) (m-new rr))))
  (define row->fs-mod
    (let ((m-new (compose1 string->number* (idx->getter title* key:fs-mod))))
      (lambda (rr) (m-new rr))))
  (define row->te-region
    (let ((m-new (compose1 string->number* (idx->getter title* key:te-region))))
      (lambda (rr) (m-new rr))))
  (define row->fs-region
    (let ((m-new (compose1 string->number* (idx->getter title* key:fs-region))))
      (lambda (rr) (m-new rr))))
  (define row->te-stx-region
    (let ((m-new (compose1 string->number* (idx->getter title* key:curr-stx-region))))
      (lambda (rr) (m-new rr))))
  (define row->olde-te-region
    (let ((m-new (compose1 string->number* (idx->getter title* key:old-te-region))))
      (lambda (rr) (m-new rr))))
  (define row->olde-fs-region
    (let ((m-new (compose1 string->number* (idx->getter title* key:old-fs-region))))
      (lambda (rr) (m-new rr))))
  (define row->olde-te-stx-region
    (let ((m-new (compose1 string->number* (idx->getter title* key:olde-stx-region))))
      (lambda (rr) (m-new rr))))
  ;; === total counts
  (let* ()
    (for ((rr (in-list row*)))
      (hash-add1! dd# (row->mode rr))
      (when (eq? 'true (row->switch rr))
        (hash-add1! dd# key:switchedmodule))
      (hash-++! dd# key:te (row->te rr))
      (hash-++! dd# key:fs (row->fs rr))
      (hash-++! dd# key:te-mod (row->te-mod rr))
      (hash-++! dd# key:fs-mod (row->fs-mod rr))
      (hash-++! dd# key:te-region (row->te-region rr))
      (hash-++! dd# key:curr-stx-region (row->te-stx-region rr))
      (hash-++! dd# key:fs-region (row->fs-region rr))
      (hash-++! dd# key:old-te-region (row->olde-te-region rr))
      (hash-++! dd# key:olde-stx-region (row->olde-te-stx-region rr))
      (hash-++! dd# key:old-fs-region (row->olde-fs-region rr))
      (void)))
  ;; === session counts
  (let* ((nsession (length timeline*))
         (*num-multi (box 0))
         (*num-up   (box 0))
         (*num-down (box 0))
         (m# (for/fold ((acc (hash)))
                       ((timeline (in-list timeline*)))
               (define mode+mod* (map (lambda (rr) (cons (row->mode rr) (row->switch rr))) timeline))
               (define uniq-mode* (remove-duplicates mode+mod* eq? #:key car))
               (cond
                 [(null? (cdr uniq-mode*))
                  ;; one mode across whole timeline
                  (hash-add1 acc (caar uniq-mode*))]
                 [else
                  (define multi-mode?
                    (let loop ((curr-mode (caar mode+mod*))
                               (any-multi? #f)
                               (mm* (cdr mode+mod*)))
                      (cond
                        [(null? mm*)
                         any-multi?]
                        [(eq? curr-mode (caar mm*))
                         (loop curr-mode any-multi? (cdr mm*))]
                        [else
                         (define switch? (eq? 'true (cdar mm*)))
                         (define next-mode (caar mm*))
                         (unless switch?
                           (if (mode<? curr-mode next-mode)
                             (set-box! *num-up (+ 1 (unbox *num-up)))
                             (set-box! *num-down (+ 1 (unbox *num-down)))))
                         (loop next-mode (or any-multi? switch?) (cdr mm*))])))
                  (when multi-mode?
                    (set-box! *num-multi (+ 1 (unbox *num-multi))))
                  acc]))))
     (hash-++! tt# 'nocheck (hash-ref m# 'nocheck 0))
     (hash-++! tt# 'nonstrict (hash-ref m# 'nonstrict 0))
     (hash-++! tt# 'num-multi (unbox *num-multi))
     (hash-++! tt# 'strict (hash-ref m# 'strict 0))
     (hash-++! tt# 'nsession nsession)
     (hash-++! tt# 'n-up (unbox *num-up))
     (hash-++! tt# 'n-down (unbox *num-down)))
  (void))

(define (group-sessions dd tt)
  (log-luau-info "group sessions ...")
  (define title* (first dd))
  (define timeline* (cdr tt))
  (define row->mode (compose1 string->symbol (idx->getter title* key:mode)))
  (define row->switch (compose1 string->symbol (idx->getter title* key:switchedmodule)))
  (define gg# (make-hash '((strict . ()) (nonstrict . ()) (nocheck . ())
                           (multimod . ()) (hasup . ()) (hasdown . ()) (other . ()))))
  (void
    (for ((timeline (in-list timeline*)))
      (define mode+mod* (map (lambda (rr) (cons (row->mode rr) (row->switch rr))) timeline))
      (define uniq-mode* (remove-duplicates mode+mod* eq? #:key car))
      (cond
        [(null? (cdr uniq-mode*))
         ;; one mode across whole timeline
         (hash-cons! gg# (caar uniq-mode*) timeline)]
        [else
         (define-values [multi-mode? has-up? has-down?]
           (let loop ((curr-mode (caar mode+mod*))
                      (any-multi? #f)
                      (any-up? #f)
                      (any-down? #f)
                      (mm* (cdr mode+mod*)))
             (cond
               [(null? mm*)
                (values any-multi? any-up? any-down?)]
               [(eq? curr-mode (caar mm*))
                (loop curr-mode any-multi? any-up? any-down? (cdr mm*))]
               [else
                (define switch? (eq? 'true (cdar mm*)))
                (define next-mode (caar mm*))
                (define next-up? (and (not switch?) (mode<? curr-mode next-mode)))
                (define next-down? (and (not switch?) (mode<? next-mode curr-mode)))
                (loop next-mode
                      (or any-multi? switch?)
                      (or any-up? next-up?)
                      (or any-down? (not next-up?))
                      (cdr mm*))])))
         (when multi-mode?
           (hash-cons! gg# 'multimod timeline))
         (when has-up?
           (hash-cons! gg# 'hasup timeline))
         (when has-down?
           (hash-cons! gg# 'hasdown timeline))
         (unless (or multi-mode? has-up? has-down?)
           (hash-cons! gg# 'other timeline))
         (void)])))
     gg#)

(define (check-one-column -csv*)
  (define csv*
    (map
      (lambda (csv)
        (define-values [dd tt] (if (path-string? csv) (csv->dd-tt csv) (apply values csv)))
        dd) -csv*))
  (define row->olde (idx->getter (first (car csv*)) key:old-def))
  ;; TODO get 1006 outta there, what values in the column?
  (define seen (make-hash))
  (for* ((dd (in-list csv*))
         (row (in-list (second dd))))
    (hash-set! seen (row->olde row) #true))
  (pretty-write seen))

(define (t:session-overview dd tt)
  ;; keep it simple, one output line per session
  (define title* (first dd))
  (define timeline* (cdr tt))
  (define row->server-time (row->datetime (index-of title* key:stime)))
  (define row->client-time (row->datetime (title-index title* key:ctime)))
  (define row->modswitch? (compose1 string->bool (idx->getter title* key:switchedmodule)))
  (define row*
    (for/list ((timeline (in-list timeline*))
               (ii (in-naturals)))
      (define num-events (length timeline))
      (define server-delta (timeline-delta-overall timeline row->server-time))
      (define client-delta (timeline-delta-overall timeline row->client-time))
      #;(unless (equal? server-delta client-delta)
        ;; almost always fires!
        (printf "BEWARE timeline ~a server gap != client gap~n ~s~n ~s~n ~a~n"
                ii
                server-delta
                client-delta
                (row->server-time (car timeline))))
      (list
        client-delta
        (->posix (row->server-time (first timeline))) ; server-start
        (->posix (row->server-time (last timeline)))  ; server-end
        num-events
        (if (zero? client-delta) #f (/ num-events client-delta))
        (timeline-deltas-per-event timeline row->client-time row->modswitch?) ; client-deltas
        )))
  (displayln "(")
  (for-each writeln (sort row* > #:key car))
  (displayln ")")
  (void))

(define (t:count-te-region dd)
  (define title* (first dd))
  (define row* (second dd))
  (define key*
    (cons key:too-complex
          (for/list ((tt (in-list title*))
                     #:when (regexp-match? #rx"TypeError1...InEditRegion" tt))
            tt)))
  (define getter* (map (lambda (kk) (compose1 string->number* (idx->getter title* kk))) key*))
  (define acc (make-hash))
  (for ((rr (in-list row*)))
    (for ((kk (in-list key*))
          (gg (in-list getter*)))
      (hash-++! acc kk (gg rr))))
  (pretty-write acc)
  (void))

(define (t:count-te-region/session title* row**)
  (define key*
    (cons key:too-complex
          (for/list ((tt (in-list title*))
                     #:when (regexp-match? #rx"TypeError1...InEditRegion" tt))
            tt)))
  (define getter* (map (lambda (kk) (compose1 string->number* (idx->getter title* kk))) key*))
  (define acc (make-hash))
  (for* ((row* (in-list row**))
         (rr (in-list row*)))
    (for ((kk (in-list key*))
          (gg (in-list getter*)))
      (hash-++! acc kk (gg rr))))
  (pretty-write acc)
  (void))

(define (query-stream csv qq rr)
  (with-input-from-file
    csv
    (lambda ()
      (define title* (map unstr (comma-split (read-line))))
      (define num-col (length title*))
      (define qq+ (qq title*))
      (define rr+ (rr title*))
      (for ((ln (in-lines)))
        (define row (parse-row ln num-col))
        (when (qq+ row)
          (writeln (rr+ row)))
        (void)))))

(define (huge-lines? title*)
  (define row->lines (compose1 string->number* (idx->getter title* key:lines)))
  (lambda (rr)
    (< 1000000 (row->lines rr))))

(define (huge-files? title*)
  (define row->files (compose1 string->number* (idx->getter title* key:files)))
  (lambda (rr)
    (< 40000   (row->files rr))))

(define (huge-ctime? title*)
  (define row->ctime (compose1 ->posix (row->datetime (title-index title* key:ctime))))
  (lambda (rr*)
    (and (not (null? rr*))
         (not (null? (cdr rr*)))
         (< 200000
            ;; posix = seconds
            ;; 2 days = (* 2 24 60 60) s = 172800
            (- (row->ctime (last rr*))
               (row->ctime (first rr*)))))))

(define (huge-ctime-render title*)
  (define row->session (compose1 unstr (idx->getter title* key:sessionid)))
  (define row->mode (compose1 string->symbol (idx->getter title* key:mode)))
  (define row->ctime (compose1 ->posix (row->datetime (title-index title* key:ctime))))
  (lambda (rr*)
    (define ss (row->session (car rr*)))
    (define mm (hash-keys (for/hash ((rr (in-list rr*))) (values (row->mode rr) #true))))
    (define tdiff
            (- (row->ctime (last rr*))
               (row->ctime (first rr*))))
    (list ss mm tdiff)))

(define (te-action-at-distance? title*)
  (define row->mode (compose1 string->symbol (idx->getter title* key:mode)))
  (define row->olde-edit (compose1 string->number* (idx->getter title* key:old-te-region)))
  (define row->curr-edit (compose1 string->number* (idx->getter title* key:te-region)))
  (define row->olde-mod  (compose1 string->number* (idx->getter title* key:old-te-mod)))
  (define row->curr-mod  (compose1 string->number* (idx->getter title* key:te-mod)))
  (lambda (rr)
    (and (= (row->olde-edit rr) (row->curr-edit rr))
         (not (= (row->olde-mod  rr) (row->curr-mod  rr))))))

(define (fs-action-at-distance? title*)
  (define row->mode (compose1 string->symbol (idx->getter title* key:mode)))
  (define row->olde-edit (compose1 string->number* (idx->getter title* key:old-fs-region)))
  (define row->curr-edit (compose1 string->number* (idx->getter title* key:fs-region)))
  (define row->olde-mod  (compose1 string->number* (idx->getter title* key:old-fs-mod)))
  (define row->curr-mod  (compose1 string->number* (idx->getter title* key:fs-mod)))
  (lambda (rr)
    (and (= (row->olde-edit rr) (row->curr-edit rr))
         (not (= (row->olde-mod  rr) (row->curr-mod  rr))))))

(define (editsize-wrong? title*)
  #;(define lines (compose1 string->number* (idx->getter title* key:lines)))
  (define edit-range (compose1 string->number* (idx->getter title* key:edits)))
  (lambda (row)
    (< 4000000000 (edit-range row))
    #;(< (lines row) (edit-range row))))

(define (editsize-negative? title*)
  ;; Never happens. Substraction underflows and the number gets huge!
  (define edit-range (compose1 string->number* (idx->getter title* key:edits)))
  (lambda (row)
    (< (edit-range row) 0)))

(define (editsize-render title*)
  (define row->session (compose1 unstr (idx->getter title* key:sessionid)))
  (define files (compose1 string->number* (idx->getter title* key:files)))
  (define lines (compose1 string->number* (idx->getter title* key:lines)))
  (define edit-range (compose1 string->number* (idx->getter title* key:edits)))
  (define row->mode (compose1 string->symbol (idx->getter title* key:mode)))
  (lambda (rr)
    (list (row->session rr)
          (row->mode rr)
          (format "files: ~a" (files rr))
          (format "lines: ~a" (files rr))
          (format "edit range: ~a" (edit-range rr)) )))

(define (negedit-render title*)
  (define row->session (compose1 unstr (idx->getter title* key:sessionid)))
  (define edit-range (compose1 string->number* (idx->getter title* key:edits)))
  (define row->mode (compose1 string->symbol (idx->getter title* key:mode)))
  (lambda (rr)
    (list (row->session rr)
          (row->mode rr)
          (format "edit range: ~a" (edit-range rr)) )))

(define (error-density-render title*)
  (define row->ctime (compose1 ->posix (row->datetime (title-index title* key:ctime))))
  (define row->te (compose1 string->number* (idx->getter title* key:te)))
  (define row->fs (compose1 string->number* (idx->getter title* key:fs)))
  (define row->te-mod (compose1 string->number* (idx->getter title* key:te-mod)))
  (define row->fs-mod (compose1 string->number* (idx->getter title* key:fs-mod)))
  (define row->te-edits (compose1 string->number* (idx->getter title* key:te-region)))
  (define row->fs-edits (compose1 string->number* (idx->getter title* key:fs-region)))
  (define row->lines (compose1 string->number* (idx->getter title* key:lines)))
  (define row->edit-range (compose1 string->number* (idx->getter title* key:edits)))
  (define row->switch (compose1 string->bool (idx->getter title* key:switchedmodule)))
  (define row->mode (compose1 string->symbol (idx->getter title* key:mode)))
  (define f*
    (list row->te row->fs row->te-mod row->fs-mod row->te-edits row->fs-edits
          row->lines row->edit-range row->switch row->ctime row->mode))
  (lambda (rr)
    (for/list ((f (in-list f*))) (f rr))))

(define (oldecurr-render title*)
  (define row->curr-te-edits (compose1 string->number* (idx->getter title* key:te-region)))
  (define row->curr-fs-edits (compose1 string->number* (idx->getter title* key:fs-region)))
  (define row->olde-te-edits (compose1 string->number* (idx->getter title* key:old-te-region)))
  (define row->olde-fs-edits (compose1 string->number* (idx->getter title* key:old-fs-region)))
  (define f*
    (list row->curr-te-edits
          row->curr-fs-edits
          row->olde-te-edits
          row->olde-fs-edits))
  (lambda (rr)
    (for/list ((f (in-list f*))) (f rr))))

(define (huge-lines-render title*)
  (define row->session (compose1 unstr (idx->getter title* key:sessionid)))
  (define row->mode (compose1 string->symbol (idx->getter title* key:mode)))
  (define row->ctime (compose1 ->posix (row->datetime (title-index title* key:ctime))))
  (define row->switch (compose1 string->bool (idx->getter title* key:switchedmodule)))
  (define row->files (compose1 string->number* (idx->getter title* key:files)))
  (define row->lines (compose1 string->number* (idx->getter title* key:lines)))
  (define row->editrange (compose1 string->number* (idx->getter title* key:edits)))
  (lambda (rr)
    (list (row->session rr)
          (row->mode rr)
          (row->switch rr)
          (row->ctime rr)
          (row->files rr)
          (row->lines rr)
          (row->editrange rr))))

(define (te-aad-render title*)
  (define row->mode (compose1 string->symbol (idx->getter title* key:mode)))
  (define row->olde-edit (compose1 string->number* (idx->getter title* key:old-te-region)))
  (define row->curr-edit (compose1 string->number* (idx->getter title* key:te-region)))
  (define row->olde-mod  (compose1 string->number* (idx->getter title* key:old-te-mod)))
  (define row->curr-mod  (compose1 string->number* (idx->getter title* key:te-mod)))
  (lambda (rr)
    (list (row->mode rr)
          (row->olde-mod rr)
          (row->curr-mod rr)
          (row->curr-edit rr))))

(define (fs-aad-render title*)
  (define row->mode (compose1 string->symbol (idx->getter title* key:mode)))
  (define row->olde-edit (compose1 string->number* (idx->getter title* key:old-fs-region)))
  (define row->curr-edit (compose1 string->number* (idx->getter title* key:fs-region)))
  (define row->olde-mod  (compose1 string->number* (idx->getter title* key:old-fs-mod)))
  (define row->curr-mod  (compose1 string->number* (idx->getter title* key:fs-mod)))
  (lambda (rr)
    (list (row->mode rr)
          (row->olde-mod rr)
          (row->curr-mod rr)
          (row->curr-edit rr))))

(define (ctc-session? title*)
  (define session*
    ;; precompute: these sesssions all have at least one CodeTooComplex error
    '("980,835,567,637,391" "607,632,332,499,626" "315,096,293,918,073"))
  (define getter (compose1 unstr (idx->getter title* key:sessionid)))
  (lambda (row)
    (member (getter row) session*)))

(define (ctc-render title*)
  (define row->ctc (compose1 string->number* (idx->getter title* key:too-complex)))
  (define row->session (compose1 unstr (idx->getter title* key:sessionid)))
  (define row->ctime (row->datetime (title-index title* key:ctime)))
  (define row->stime (row->datetime (title-index title* key:stime)))
  (define row->mode (compose1 string->symbol (idx->getter title* key:mode)))
  (lambda (rr)
    (list (row->session rr)
          (row->ctime rr)
          (row->stime rr)
          (row->mode rr)
          (row->ctc rr))))

(define (string->bool str)
  (case str
   (("true") #true)
   (("false") #false)
   (else (raise-argument-error 'string->bool "(or/c \"true\" \"false\")" str))))

(define (timeline-delta-overall timeline row->dt)
  (define dt0 (row->dt (first timeline)))
  (define dt1 (row->dt (last timeline)))
  (milliseconds-between dt0 dt1))

(define (timeline-deltas-per-event timeline row->dt row->modswitch?)
  (ms-time-deltas (map (lambda (rr) (list (row->dt rr) (row->modswitch? rr))) timeline)))

(define (ms-time-deltas time*0)
  (let loop ([time* time*0])
    (if (null? (cdr time*))
      '()
      (let* ((gap (milliseconds-between (car (car time*)) (car (cadr time*))))
             (switch? (cadr (cadr time*))))
        (cons (if switch? (list gap #true) gap)
              (loop (cdr time*)))))))

(define (minutes->ms mm)
  (seconds->ms (* mm 60)))

(define (seconds->ms ss)
  (* ss 1000))

(define (pct a b)
  (format "~a%" (rnd (* 100 (/ a b)))))

(define (session-gaps ii*)
  (define all-gap*
    (for*/list ((ii (in-list ii*))
                (dd (in-list (last ii))))
      dd))
  (define total-num (length all-gap*))
  (define nomod-gap* (filter real? all-gap*))
  (define nomod-num (length nomod-gap*))
  (define (make-row* cmp min*)
    (for/list ((big-gap (in-list min*)))
      (define ms (minutes->ms big-gap))
      (define num
        (for*/sum ((delta (in-list nomod-gap*))
                   #:when (cmp delta ms))
          1))
      (define cmp-name (object-name cmp))
      (list
        (if (< big-gap 60)
          (if (< big-gap 1)
            (format "~a~a sec" cmp-name (* big-gap 60))
            (format "~a~a min" cmp-name  big-gap))
          (format "~a~a hr" cmp-name (quotient big-gap 60)))
        num
        (pct num nomod-num))))
  (define lo-row*
    (make-row* < (list 1/60 2/60 5/60)))
  (define hi-row*
    (make-row* > (list 1 5 10 30 60 (* 60 2) (* 60 6) (* 60 12) (* 60 24))))
  (printf "~a total~n" total-num)
  (printf "~a mod switches~n" (- total-num nomod-num))
  (print-simple-table
    #:align '(left right)
    (cons (list "gap-size" "count" "%total")
      (append lo-row* hi-row*)))
  (void))

#;
 (define (gq1-analyze dd tt)
   (printf "=== sanity checks~n")
   (define title* (first dd))
   (define row* (second dd))
   (define timeline* (cdr tt))
   ;;(printf " distribution of edit region size:~n")
   ;;(let* ((edit-key key:edits)
   ;;       (edit-idx (title-index title* edit-key))
   ;;       (edit-size# (for/fold ((acc (hash)))
   ;;                             ((rr (in-list row*)))
   ;;                     (hash-add1 acc (string->number* (list-ref rr edit-idx))))))
   ;;  (if (and (= 1 (hash-count edit-size#))
   ;;           (hash-has-key? edit-size# 0))
   ;;    (printf "  all regions have size 0~n")
   ;;    (printf "  not implemented~n")))
   (printf " session length vs. num sessions:~n")
   (let ((h# (make-hash)))
     (for ((tt (in-list timeline*)))
       (hash-add1! h# (length tt)))
     (print-distribution h# #:rev #true)
     (void))
   (newline)
   (printf " how long between logs:~n")
   (define same-instant# (make-hash))
   (let* ((k* (map (lambda (n) (* n 1000)) '(0 1/200 1/20 1/2 1 2 5 10 30 60 120 300 600 3600))) ;; milliseconds
          (h# (make-hash (map (lambda (x) (cons x 0)) k*))))
     (define time-key key:ctime)
     (define time-idx (title-index title* time-key))
     (define per-timeline-mean* (box '()))
     (define per-timeline-median* (box '()))
     (for ((tt (in-list timeline*))
           #:when (len>1 tt))
       (define dd* (ms-time-deltas (map (lambda (r) (list-ref r time-idx)) tt)))
       (set-box! per-timeline-mean* (cons (mean dd*) (unbox per-timeline-mean*)))
       (set-box! per-timeline-median* (cons (median < dd*) (unbox per-timeline-median*)))
       (for ((dd (in-list dd*))
             (dd_idx (in-naturals)))
         (or
           (for/first ((k (in-list k*))
                       #:when (<= dd k))
             (when (zero? k)
               (define l0 (list-ref tt dd_idx))
               (define l1 (list-ref tt (+ 1 dd_idx)))
               (define kk (ts->datetime (list-ref l0 time-idx)))
               (hash-update! same-instant# kk (lambda (x) (list* l0 l1 x)) (lambda () '())))
             (hash-add1! h# k))
           (hash-add1! h# '+1hr))
         (void)))
     (for ((k (in-list k*)))
       (printf "  <=~ams: ~a~n" k (hash-ref h# k))
       (void))
     (printf "  +1hr: ~a~n" (hash-ref h# '+1hr))
     (printf "~n~n")
     (printf " per session median deltas (ms), summary stats:~n")
     (let* ((pt* (sort (unbox per-timeline-median*) <)))
       (printf "  ~a~n" (mmmm pt*))
       (void))
     (printf " per session avg deltas (ms), summary stats:~n")
     (let* ((pt* (sort (unbox per-timeline-mean*) <)))
       (printf "  ~a~n" (mmmm pt*))
       (void))
     (void))
   (newline)
   (printf " logs at same instant~n")
   (let* ((len* (map length (hash-values same-instant#))))
     (printf "  ~a total logs~n" (apply + len*))
     (printf "  ~a groups~n" (hash-count same-instant#))
     (printf "  group sizes: ~a~n" (sort len* >))
     (printf "  times: ~a~n" (hash-keys same-instant#))
     (void))
   (newline)
   (void))

(define (make-size-distribution filename*)
      (define size# (make-hash))
      (define title* (csv->title* "out/full-dataset.csv"))
      (for ((fn (in-list filename*)))
        (define row->ctime (row->datetime (title-index title* key:ctime)))
        (define row->files (compose1 string->number* (idx->getter title* key:files)))
        (define row->lines (compose1 string->number* (idx->getter title* key:lines)))
        (define row->editrange (compose1 string->number* (idx->getter title* key:edits)))
        (define row*->event-count length)
        (define (row*->session-timespan row*)
          (cond
           [(null? (cdr row*))
            #f]
           [else
            (define start-time (row->ctime (first row*)))
            (define end-time (row->ctime (last row*)))
            (milliseconds-between start-time end-time)]))
        (for ((row* (in-list (file->value fn))))
          (for ((kk (in-list '(event-count timespan)))
                (ff (in-list (list row*->event-count row*->session-timespan))))
            (hash-update! size# kk
              (lambda (hh) (hash-add1 hh (ff row*)))
              (lambda () (hash))))
          (for ((row (in-list row*)))
            (for ((kk (in-list '(files lines editrange)))
                  (ff (in-list (list row->files row->lines row->editrange))))
              (hash-update! size# kk
                (lambda (hh) (hash-add1 hh (ff row)))
                (lambda () (hash)))))
          (void)))
  size#)

(define (plot-explore title* fn*)
  (define row->ctime (row->datetime (title-index title* key:ctime)))
  (define row->stime (row->datetime (title-index title* key:stime)))
  (define row->files (compose1 string->number* (idx->getter title* key:files)))
  (define row->lines (compose1 string->number* (idx->getter title* key:lines)))
  (define row->editrange (compose1 string->number* (idx->getter title* key:edits)))
  (define row->te (compose1 string->number* (idx->getter title* key:te)))
  (define row->fs (compose1 string->number* (idx->getter title* key:fs)))

  (define row->session (compose1 unstr (idx->getter title* key:sessionid)))
  (define row->te-region
    (let ((m-new (compose1 string->number* (idx->getter title* key:te-region))))
      (lambda (rr) (m-new rr))))
  (define row->fs-region
    (let ((m-new (compose1 string->number* (idx->getter title* key:fs-region))))
      (lambda (rr) (m-new rr))))
  (define row->te-mod
    (let ((m-new (compose1 string->number* (idx->getter title* key:te-mod))))
      (lambda (rr) (m-new rr))))
  (define row->fs-mod
    (let ((m-new (compose1 string->number* (idx->getter title* key:fs-mod))))
      (lambda (rr) (m-new rr))))
  (define row->mode (compose1 string->symbol (idx->getter title* key:mode)))
  (define row->switch (compose1 string->symbol (idx->getter title* key:switchedmodule)))

  (define distro-maker*
    (list
;;      (list "client time"
;;        (lambda (row*)
;;          (map (compose1 ->posix row->ctime) row*)))
;;      (list "server time"
;;        (lambda (row*)
;;          (map (compose1 ->posix row->stime) row*)))
;;      (list "event count"
;;        length)
;;      (list "zoom event count"
;;        length
;;        (f:plot-session-distro #:x-max 100 #:y-max 80))
;;      (list "session c.time span"
;;        (lambda (row*)
;;          (define ct* (map row->ctime row*))
;;          (define ms (milliseconds-between (first ct*) (last ct*)))
;;          ms)
;;          )
;;      (list "zoom session c.t.span"
;;        (lambda (row*)
;;          (define ct* (map row->ctime row*))
;;          (define ms (milliseconds-between (first ct*) (last ct*)))
;;          (if (< ms (* 5 60 1000))
;;            ms
;;            #f))
;;        (f:plot-session-distro #:x-max 60000 #:y-max 20))
;;      (list "median files per session"
;;        (lambda (row*)
;;          (median < (map row->files row*))))
;;      (list "zoom median files per session"
;;        (lambda (row*)
;;          (median < (map row->files row*)))
;;        (f:plot-session-distro #:x-max 210 #:y-max 40))
;;      (list "median total lines per session"
;;        (lambda (row*)
;;          (median < (map row->lines row*))))
;;      (list "median editRange lines per session"
;;        (lambda (row*)
;;          (median < (map row->editrange row*))))
;;      (list "zoom median editRange lines per session"
;;        (lambda (row*)
;;          (median < (map row->editrange row*)))
;;        (f:plot-session-distro #:x-max 100 #:y-max 60))
;;      (list "c.t gaps"
;;        (lambda (row*)
;;          (if (or (null? row*) (null? (cdr row*)))
;;            #f
;;            (median < (timeline-deltas-per-event row* row->ctime (lambda (x) #false))))))

      (list "median total TE / lines density"
        (lambda (row*)
          (median < (map (lambda (rr) (/zero (row->te rr) (row->lines rr))) row*))))
      (list "median total FS / lines density"
        (lambda (row*)
          (median < (map (lambda (rr) (/zero (row->fs rr) (row->lines rr))) row*))))
      (list "median edit range TE density"
        (lambda (row*)
          (median < (map (lambda (rr) (/zero (row->te-region rr) (row->editrange rr))) row*))))
      (list "median edit range FS density"
        (lambda (row*)
          (median < (map (lambda (rr) (/zero (row->fs-region rr) (row->editrange rr))) row*))))

      (list "max total TE / lines density"
        (lambda (row*)
          (apply max (map (lambda (rr) (/zero (row->te rr) (row->lines rr))) row*))))
      (list "max total FS / lines density"
        (lambda (row*)
          (apply max (map (lambda (rr) (/zero (row->fs rr) (row->lines rr))) row*))))
      (list "max edit range TE density"
        (lambda (row*)
          (apply max (map (lambda (rr) (/zero (row->te-region rr) (row->editrange rr))) row*))))
      (list "max edit range FS density"
        (lambda (row*)
          (apply max (map (lambda (rr) (/zero (row->fs-region rr) (row->editrange rr))) row*))))

      (list "min total TE / lines density"
        (lambda (row*)
          (apply min (map (lambda (rr) (/zero (row->te rr) (row->lines rr))) row*))))
      (list "min total FS / lines density"
        (lambda (row*)
          (apply min (map (lambda (rr) (/zero (row->fs rr) (row->lines rr))) row*))))
      (list "min edit range TE density"
        (lambda (row*)
          (apply min (map (lambda (rr) (/zero (row->te-region rr) (row->editrange rr))) row*))))
      (list "min edit range FS density"
        (lambda (row*)
          (apply min (map (lambda (rr) (/zero (row->fs-region rr) (row->editrange rr))) row*))))

;      (list "T+F error"
;        (error-lines2 (compose1 ->posix row->ctime) row->te row->fs)
;        (f:plot-lines))
;      (list "type error"
;        (error-lines (compose1 ->posix row->ctime) row->te)
;        (f:plot-lines))
;      (list "forced strict"
;        (error-lines (compose1 ->posix row->ctime) row->fs)
;        (f:plot-lines))
;      (list "delta type error"
;        (delta-timeline-points (compose1 ->posix row->ctime) row->te)
;        (f:plot-lines #:dots? #true))
;      (list "delta forced strict"
;        (delta-timeline-points (compose1 ->posix row->ctime) row->fs)
;        (f:plot-lines #:dots? #true))

      ;; ---
;      (list "zoom T+F"
;        (error-lines2 (compose1 ->posix row->ctime) row->te row->fs)
;        (f:plot-lines #:x-min 0 #:x-max 12000 #:y-min 0 #:y-max 100))
;      (list "zoom type error"
;        (error-lines (compose1 ->posix row->ctime) row->te)
;        (f:plot-lines #:x-min 0 #:x-max 30000 #:y-min 0 #:y-max 100))
;      (list "zoom forced strict"
;        (error-lines (compose1 ->posix row->ctime) row->fs)
;        (f:plot-lines #:x-min 0 #:x-max 30000 #:y-min 0 #:y-max 100))
;;      (list "zoom delta type error"
;;        (delta-timeline-points (compose1 ->posix row->ctime) row->te)
;;        (f:plot-lines #:dots? #true #:x-min 0 #:x-max 30000 #:y-min -100 #:y-max 100))
;;      (list "zoom delta forced strict"
;;        (delta-timeline-points (compose1 ->posix row->ctime) row->fs)
;;        (f:plot-lines #:dots? #true #:x-min 0 #:x-max 30000 #:y-min -100 #:y-max 100))

;      (list "density: total TE / lines"
;        (error-lines (compose1 ->posix row->ctime)
;          (lambda (rr) (/zero (row->te rr) (row->lines rr))))
;        (f:plot-lines #:dots? #true))
;      (list "density: total FS / lines"
;        (error-lines (compose1 ->posix row->ctime)
;          (lambda (rr) (/zero (row->fs rr) (row->lines rr))))
;        (f:plot-lines #:dots? #true))
;      (list "density: edit TE / lines"
;        (error-lines (compose1 ->posix row->ctime)
;          (lambda (rr) (/zero (row->te-region rr) (row->editrange rr))))
;        (f:plot-lines #:dots? #true))
;      (list "density: edit FS / lines"
;        (error-lines (compose1 ->posix row->ctime)
;          (lambda (rr) (/zero (row->fs-region rr) (row->editrange rr))))
;        (f:plot-lines #:dots? #true))

      ;; ---
;      (list "TODO zoom density"
;        (error-lines2 (compose1 ->posix row->ctime) row->te row->fs)
;        (f:plot-lines #:x-min 0 #:x-max 12000 #:y-min 0 #:y-max 100))

;      (list "HUGE TE <= FS"
;        (event-vector (lambda (rr)
;          (define vv (<= (row->te rr) (row->fs rr)))
;            (unless vv
;              (displayln
;                (list (row->session rr)
;                      (row->te rr) (row->fs rr)
;                      (row->te-region rr) (row->fs-region rr)
;                      (row->te-mod rr) (row->fs-mod rr)
;                      (row->switch rr )
;                      (row->mode rr)
;                      rr)))
;          vv))
;        ;; (event-vector (lambda (rr) (<= (row->te rr) (row->fs rr))))
;        (f:plot-event-bars))
;      (list "TE <= FS"
;        (event-vector (lambda (rr) (<= (row->te rr) (row->fs rr))))
;        (f:plot-event-bars #:x-min 0 #:x-max 100))
;      (list "zoom TE <= FS"
;        (event-vector (lambda (rr) (<= (row->te rr) (row->fs rr))))
;        (f:plot-event-bars #:x-min 0 #:x-max 100 #:y-min 0 #:y-max 14))

;      (list "up/down match: TE FS"
;        (event-diff-vector
;          (lambda (rr0 rr1)
;            (define te-gap (- (row->te rr0) (row->te rr1)))
;            (define fs-gap (- (row->fs rr0) (row->fs rr1)))
;            (if (= te-gap fs-gap 0)
;              0
;              (or (and (positive? te-gap) (positive? fs-gap))
;                  (and (negative? te-gap) (negative? fs-gap))))))
;        (f:plot-event-bars #:x-min 0 #:x-max 100))
;      (list "TE down?"
;        (event-diff-vector
;          (lambda (rr0 rr1)
;            (define v0 (row->te rr0))
;            (define v1 (row->te rr1))
;            (if (= v0 v1) 0 (< v1 v0))))
;        (f:plot-event-bars #:x-min 0 #:x-max 100))
;      (list "FS down?"
;        (event-diff-vector
;          (lambda (rr0 rr1)
;            (define v0 (row->fs rr0))
;            (define v1 (row->fs rr1))
;            (if (= v0 v1) 0 (< v1 v0))))
;        (f:plot-event-bars #:x-min 0 #:x-max 100))

;      (list "TE < 1st point"
;        (event-vector/acc
;            #f
;            (lambda (rr acc)
;              (define curr-te (row->te rr))
;              (define first-te (or acc curr-te))
;              (values (if (= curr-te first-te) 0 (< curr-te first-te)) first-te)))
;        (f:plot-event-bars #:x-min 0 #:x-max 100 ;; #:y-min 0 #:y-max 14
;        ))
;      (list "FS < 1st point"
;        (event-vector/acc
;            #f
;            (lambda (rr acc)
;              (define curr-fs (row->fs rr))
;              (define first-fs (or acc curr-fs))
;              (values (if (= curr-fs first-fs) 0 (< curr-fs first-fs)) first-fs)))
;        (f:plot-event-bars #:x-min 0 #:x-max 100 ;;#:y-min 0 #:y-max 14
;        ))

;      (list "TE FS vectors"
;          (event-diff-vector
;            (lambda (rr0 rr1)
;              (define te0 (row->te rr0))
;              (define te1 (row->te rr1))
;              (define fs0 (row->fs rr0))
;              (define fs1 (row->fs rr1))
;              (vector (vector te0 te1) (vector fs0 fs1))))
;        (f:plot-vectors #:corr? #true #:summary? #true))
;      (list "TE FS vec, yes switch"
;          (event-diff-vector #:filter? #true
;            (lambda (rr0 rr1)
;              (define switch? (eq? 'true (row->switch rr1)))
;              (if switch?
;                (let ()
;                  (define te0 (row->te rr0))
;                  (define te1 (row->te rr1))
;                  (define fs0 (row->fs rr0))
;                  (define fs1 (row->fs rr1))
;                  (vector (vector te0 te1) (vector fs0 fs1)))
;                #false)))
;        (f:plot-vectors #:corr? #true #:summary? #true))
;      (list "TE FS vec, no switch"
;          (event-diff-vector #:filter? #true
;            (lambda (rr0 rr1)
;              (define switch? (eq? 'true (row->switch rr1)))
;              (if switch?
;                #false
;                (let ()
;                  (define te0 (row->te rr0))
;                  (define te1 (row->te rr1))
;                  (define fs0 (row->fs rr0))
;                  (define fs1 (row->fs rr1))
;                  (vector (vector te0 te1) (vector fs0 fs1))))))
;        (f:plot-vectors #:corr? #true #:summary? #true))

;;      #;(list "TE FS zoom"
;;          (event-diff-vector
;;            (lambda (rr0 rr1)
;;              (define te0 (row->te rr0))
;;              (define te1 (row->te rr1))
;;              (define fs0 (row->fs rr0))
;;              (define fs1 (row->fs rr1))
;;              (vector (vector te0 te1) (vector fs0 fs1))))
;;        (f:plot-vectors #:x-min 0 #:x-max 100 #:y-min 0 #:y-max 100))
;;      (list "TE FS scale"
;;          (event-diff-vector
;;            (lambda (rr0 rr1)
;;              (define te0 (row->te rr0))
;;              (define te1 (row->te rr1))
;;              (define te-max (max te0 te1))
;;              (define fs0 (row->fs rr0))
;;              (define fs1 (row->fs rr1))
;;              (define fs-max (max fs0 fs1))
;;              (define (div a b) (if (zero? b) 0 (/ a b)))
;;              (vector (vector (div te0 te-max) (div te1 te-max)) (vector (div fs0 fs-max) (div fs1 fs-max)))))
;;        (f:plot-vectors ))

;      (list "cutoff?"
;        (let* ((lo-time (->posix (ts->datetime min-timestamp)))
;               (hi-time (->posix (ts->datetime max-timestamp))))
;          (lambda (row*)
;            ;; CUTOFF cutoff discard row when ...
;            (if (or (null? row*) (null? (cdr row*)))
;              #f
;              (let* ((t* (map (compose1 ->posix row->ctime) row*))
;                     (t0 (first t*))
;                     (tN (last t*))
;                     (gap* (let loop ((tt* t*))
;                             (cons (- (second tt*) (first tt*))
;                                   (if (null? (cddr tt*))
;                                     '()
;                                     (loop (cdr tt*))))))
;                     (avg-gap (mean gap*))
;                     (med-gap (median < gap*))
;                     (multiplier 4)
;                     (the-gap (* multiplier (max avg-gap med-gap))))
;                (if (or (< (- t0 the-gap) lo-time)
;                        (< hi-time (+ tN the-gap)))
;                  (begin (printf "CUTOFF~n") 10) ;; yes cutoff
;                  0))))))

;;      (list "start > end?"
;;        (lambda (row*)
;;          (if (or (null? row*) (null? (cdr row*)))
;;            #f
;;            (let* ((r0 (first row*))
;;                   (rN (last row*))
;;                   (te0 (row->te r0))
;;                   (te1 (row->te rN)))
;;              (if (< te1 te0)
;;                10 ;; yes end better
;;                0)))))


      ))
  (define pp*
    (flatten
        (apply map list
          (cons (blank) (map (compose1 big-text first) distro-maker*))
          (for/list ((fn (in-list fn*)))
            (cons (fn->title fn)
              (for/list ((dm (in-list distro-maker*)))
                  (define pp
                    (if (null? (cddr dm))
                      ((f:plot-session-distro) fn (first dm) (second dm))
                      ((third dm) fn (first dm) (second dm))))
                  pp))))))
  (table
    (+ 1 (length fn*))
    pp*
    cc-superimpose
    cc-superimpose
    40
    10))

(define ((f:plot-session-distro #:x-min [x-min #f] #:y-min [y-min #f] #:x-max [x-max #f] #:y-max [y-max #f])  filename lbl f-row*)
  (define row** (file->value filename))
  (define num*
    (let ((vv (for*/list ((row* (in-list row**))
                          #:unless (null? row*)
                          (vv (in-value (f-row* row*)))
                          #:when vv)
                vv)))
      (if (pair? (car vv))
        (apply append vv)
        vv)))
  (f:plot-simple-distro num* #:x lbl #:time-dist (string-contains? lbl "time") #:x-min x-min #:y-min y-min #:x-max x-max #:y-max y-max))

(define ((delta-timeline-points get-x get-y) row*)
  (define min-session-length 2)
  (cond
   [(< min-session-length (length row*))
    (define x0 (get-x (first row*)))
    (define y-prev #f)
    (for/list ((rr (in-list row*)))
      (define y-curr (get-y rr))
      (define yy (if y-prev (- y-curr y-prev) 0))
      (set! y-prev y-curr)
      (vector (- (get-x rr) x0) yy))]
   [else
    #f]))

(define ((event-vector row->bool) row*)
  (begin0
    (map row->bool row*)
    #;(newline)))

(define ((event-vector/acc acc row->bool) row*)
  (define *acc (box acc))
  (for/list ((rr (in-list row*)))
    (define-values [res acc+] (row->bool rr (unbox *acc)))
    (set-box! *acc acc+)
    res))

(define ((event-diff-vector row2->bool #:filter? [filter? #f]) row*)
  (cond
   [(or (null? row*) (null? (cdr row*)))
    '()]
   [else
    (let loop ((prev (car row*))
               (rr* (cdr row*)))
      (define curr (car rr*))
      (define tl (if (null? (cdr rr*)) '() (loop curr (cdr rr*))))
      (define hd (row2->bool prev curr))
      (if (and filter? (not hd))
        tl
        (cons hd tl)))]))

(define ((error-lines get-x get-y) row*)
  #;(define min-session-length 3)
  (cond
   [#true #;(= min-session-length (length row*))
    (define x0 (get-x (first row*)))
    (for/list ((rr (in-list row*)))
      (vector (- (get-x rr) x0)
              (get-y rr)))]
   [else
    #f]))

(define ((error-lines2 get-x . get-y*) row*)
;; TODO use this instead of the original
  (define min-session-length 9)
  (cond
   [(= min-session-length (length row*))
    (define x0 (get-x (first row*)))
    (for/list ((yy (in-list get-y*)))
      (for/list ((rr (in-list row*)))
        (define x1 (- (get-x rr) x0))
        (vector x1 (yy rr))))]
   [else
    #f]))

(define ((f:plot-lines #:dots? [dots-only? #f] #:x-min [x-min #f] #:x-max [x-max #f] #:y-min [y-min #f] #:y-max [y-max #f]) filename lbl f-row*)
  (define row** (file->value filename))
  (define num**
    (for*/list ((row* (in-list row**))
                          #:unless (null? row*)
                          (vv (in-value (f-row* row*)))
                          #:when vv)
                ;; (with-output-to-file "bensession.txt" #:exists 'append (lambda () (displayln row*)))
                vv))
  (parameterize ([plot-x-far-ticks no-ticks]
                 [plot-y-far-ticks no-ticks]
                 ;;[plot-x-ticks (time-dist-ticks)]
                 [plot-x-ticks (linear-major-y-ticks 4)]
                 [plot-y-ticks (linear-major-y-ticks 3)]
                 [plot-font-size 10]
                 [plot-font-family 'roman])
    (printf "plot-pict ~a~n" lbl)
    (define *np 0)
    (define *bignp 0)
    (define *zeronp 0)
    (define my-lines
      (for/list ((num* (in-list num**))
                 (ii (in-naturals)))
        (for/list ((n* (in-list (if (pair? (car num*)) num* (list num*))))
                   ;; dot
                   (sym (in-cycle (in-list '(plus times diamond circle3 fullsquare)))))
          (for ((xy (in-list n*)))
            (define y (vector-ref xy 1))
            (set! *np (+ 1 *np))
            (set! *bignp (+ (if (< 0 y) 1 0) *bignp))
            (set! *zeronp (+ (if (= 0 y) 1 0) *zeronp))
            (void))
          (list
            (if dots-only? '() (lines n* #:color ii #:alpha 0.2))
            (points n* #:size 10 #:color ii #:alpha 0.5 #:sym sym)))))
    (plot-pict
      (list
        my-lines
        (hrule 0 #:alpha 0.5 #:color 0))
      #:width 800
      #:height 300
      #:y-max y-max
      #:y-min y-min
      #:x-max x-max
      #:x-min x-min
      #:title (format "~a points, ~a (~a%) >0, ~a (~a%) =0"
                *np
                *bignp (exact-round (* 100 (/ *bignp *np)))
                *zeronp (exact-round (* 100 (/ *zeronp *np))))
      #:y-label #f
      #:x-label #f #;lbl)))

(define ((f:plot-vectors #:corr? [corr? #false] #:summary? [summary? #false]
                         #:x-min [x-min #f] #:x-max [x-max #f]
                         #:y-min [y-min #f] #:y-max [y-max #f])
                         filename lbl f-row*)
  (define row** (file->value filename))
  (define vec*
    (for*/list ((row* (in-list row**))
                          #:unless (null? row*)
                          (vv* (in-value (f-row* row*)))
                          #:when vv*
                          (vv (in-list vv*)))
                vv))
  (define hkey* '(vertical horizontal zero I II III IV))
  (define h# (make-hash (for/list ((kk (in-list hkey*))) (cons kk 0))))
  (define (hup vec)
    (match-define (vector (vector te0 te1) (vector fs0 fs1)) vec)
    (define kk
      (cond
       [(and (= te0 te1) (= fs0 fs1))
        'zero]
       [(and (= te0 te1) (not (= fs0 fs1)))
        'vertical]
       [(and (not (= te0 te1)) (= fs0 fs1))
        'horizontal]
        ;; I II III IV
       [(and (< te0 te1) (< fs0 fs1))
        'I]
       [(and (< te0 te1) (> fs0 fs1))
        'IV]
       [(and (> te0 te1) (< fs0 fs1))
        'II]
       [(and (> te0 te1) (> fs0 fs1))
        'III]
       [else
        (raise-argument-error 'hup "unknown case ~s" vec)]))
    (hash-add1! h# kk))
  (define (hfmt kk)
    (define N (apply + (hash-values h#)))
    (define mm (hash-ref h# kk))
    (list mm (round (* 100 (/ mm N)))))
  (parameterize ([plot-x-far-ticks no-ticks]
                 [plot-y-far-ticks no-ticks]
                 ;;[plot-x-ticks (time-dist-ticks)]
                 [plot-x-ticks (linear-major-y-ticks 4)]
                 [plot-y-ticks (linear-major-y-ticks 3)]
                 [plot-font-size 10]
                 [plot-font-family 'roman])
    (void (for-each hup vec*))
    (define cc 4)
    (define elem*
      (list
        (let* ((num-vertical (hash-ref h# 'vertical))
               (num-horizontal (hash-ref h# 'horizontal))
               (num-disagree (+ (hash-ref h# 'IV) (hash-ref h# 'II)))
               (num-zero (hash-ref h# 'zero))
               (num-agree (+ (hash-ref h# 'I) (hash-ref h# 'III))))
          (discrete-histogram
            (list
              (vector "zero" num-zero)
              (vector "FS" num-vertical)
              (vector "TE" num-horizontal)
              (vector "dis." num-disagree)
              (vector "agree" num-agree))
            #:color cc
            #:line-color cc))))
    (printf "plot-pict ~a~n" lbl)
    (define pp
      (plot-pict
        elem*
        #:width 400
        #:height 400
        #:y-max y-max
        #:y-min y-min
        #:x-max x-max
        #:x-min x-min
        #:title (format "~a vectors  ~a" (length vec*)
                  (if corr? (let ()
                    (define te-gap* (map (lambda (vv) (define te (vector-ref vv 0)) (- (vector-ref te 1) (vector-ref te 0))) vec*))
                    (define fs-gap* (map (lambda (vv) (define fs (vector-ref vv 1)) (- (vector-ref fs 1) (vector-ref fs 0))) vec*))
                    (define (cmp n0 n1) (cond ((< n0 n1) 1) ((= n0 n1) 0) (else -1)))
                    (define te-dir* (map (lambda (vv) (define te (vector-ref vv 0)) (cmp (vector-ref te 0) (vector-ref te 1))) vec*))
                    (define fs-dir* (map (lambda (vv) (define fs (vector-ref vv 1)) (cmp (vector-ref fs 0) (vector-ref fs 1))) vec*))
                    (format
                      " gap corr. ~a dir corr. ~a"
                      (correlation te-gap* fs-gap*)
                      (correlation te-dir* fs-dir*))) ""))
        #:y-label "FS"
        #:x-label "TE"))
    (define summary
      (if summary?
        (ptable
          #:ncols 3
          (for/list ((kk (in-list hkey*)))
            (define vv (hfmt kk))
            (map text (list (~a kk) (~a (car vv)) (format "~a%" (cadr vv))))))
        (blank)))
    (vc-append 10 pp summary)))

;; original, draw liness
(define ((f:plot-vectors0 #:corr? [corr? #false] #:summary? [summary? #false]
                         #:x-min [x-min #f] #:x-max [x-max #f]
                         #:y-min [y-min #f] #:y-max [y-max #f])
                         filename lbl f-row*)
  (define row** (file->value filename))
  (define vec*
    (for*/list ((row* (in-list row**))
                          #:unless (null? row*)
                          (vv* (in-value (f-row* row*)))
                          #:when vv*
                          (vv (in-list vv*)))
                vv))
  (define hkey* '(vertical horizontal zero I II III IV))
  (define h# (make-hash (for/list ((kk (in-list hkey*))) (cons kk 0))))
  (define (hup vec)
    (match-define (vector (vector te0 te1) (vector fs0 fs1)) vec)
    (define kk
      (cond
       [(and (= te0 te1) (= fs0 fs1))
        'zero]
       [(and (= te0 te1) (not (= fs0 fs1)))
        'vertical]
       [(and (not (= te0 te1)) (= fs0 fs1))
        'horizontal]
        ;; I II III IV
       [(and (< te0 te1) (< fs0 fs1))
        'I]
       [(and (< te0 te1) (> fs0 fs1))
        'IV]
       [(and (> te0 te1) (< fs0 fs1))
        'II]
       [(and (> te0 te1) (> fs0 fs1))
        'III]
       [else
        (raise-argument-error 'hup "unknown case ~s" vec)]))
    (hash-add1! h# kk))
  (define (hfmt kk)
    (define N (apply + (hash-values h#)))
    (define mm (hash-ref h# kk))
    (list mm (round (* 100 (/ mm N)))))
  (parameterize ([plot-x-far-ticks no-ticks]
                 [plot-y-far-ticks no-ticks]
                 ;;[plot-x-ticks (time-dist-ticks)]
                 [plot-x-ticks (linear-major-y-ticks 4)]
                 [plot-y-ticks (linear-major-y-ticks 3)]
                 [plot-font-size 10]
                 [plot-font-family 'roman])
    (define cc 4)
    (define elem*
      (list
        (for/list ((vec (in-list vec*)))
          (hup vec)
          (define te (vector-ref vec 0))
          (define fs (vector-ref vec 1))
          (lines
            (vector
              (vector (vector-ref te 0) (vector-ref fs 0))
              (vector (vector-ref te 1) (vector-ref fs 1)))
            #:color cc
            #:alpha 0.5))
        (points
          (for/list ((vec (in-list vec*)))
            (vector (vector-ref (vector-ref vec 0) 1)
                    (vector-ref (vector-ref vec 1) 1)))
          #:sym 'dot
          #:color cc
          #:alpha 0.5
          #:size 6)))
    (printf "plot-pict ~a~n" lbl)
    (define pp
      (plot-pict
        elem*
        #:width 400
        #:height 400
        #:y-max y-max
        #:y-min y-min
        #:x-max x-max
        #:x-min x-min
        #:title (format "~a vectors  ~a" (length vec*)
                  (if corr? (let ()
                    (define te-gap* (map (lambda (vv) (define te (vector-ref vv 0)) (- (vector-ref te 1) (vector-ref te 0))) vec*))
                    (define fs-gap* (map (lambda (vv) (define fs (vector-ref vv 1)) (- (vector-ref fs 1) (vector-ref fs 0))) vec*))
                    (define (cmp n0 n1) (cond ((< n0 n1) 1) ((= n0 n1) 0) (else -1)))
                    (define te-dir* (map (lambda (vv) (define te (vector-ref vv 0)) (cmp (vector-ref te 0) (vector-ref te 1))) vec*))
                    (define fs-dir* (map (lambda (vv) (define fs (vector-ref vv 1)) (cmp (vector-ref fs 0) (vector-ref fs 1))) vec*))
                    (format
                      " gap corr. ~a dir corr. ~a"
                      (correlation te-gap* fs-gap*)
                      (correlation te-dir* fs-dir*))) ""))
        #:y-label "FS"
        #:x-label "TE"))
    (define summary
      (if summary?
        (ptable
          #:ncols 3
          (for/list ((kk (in-list hkey*)))
            (define vv (hfmt kk))
            (map text (list (~a kk) (~a (car vv)) (format "~a%" (cadr vv))))))
        (blank)))
    (vc-append 10 pp summary)))

(define (/zero a b)
  (if (zero? b)
    -1
    (/ a b)))

(define (f:plot-simple-distro num* #:x [xlbl #f] #:time-dist [time-dist #f] #:x-min [x-min #f] #:y-min [y-min #f] #:x-max [x-max #f] #:y-max [y-max #f])
  (define num# (for/fold ((acc (hash))) ((n (in-list num*))) (hash-add1 acc n)))
  (parameterize ([plot-x-far-ticks no-ticks]
                 [plot-y-far-ticks no-ticks]
                 [plot-x-ticks (if time-dist (time-dist-ticks) (plot-x-ticks))]
                 [plot-y-ticks (linear-major-y-ticks 3)]
                 [plot-font-size 10]
                 [rectangle-color bg-dark-blue]
                 [rectangle-line-color bg-dark-blue]
                 [plot-font-family 'roman])
    (printf "plot-pict ~a~n" xlbl)
    (define my-bars
      (rectangles
        (for/list (((xx yy) (in-hash num#)))
          (vector (ivl xx (+ xx 1/2))
                  (ivl 0 yy)))))
    (plot-pict
      my-bars
      #:width 400
      #:height 300
      #:x-min x-min
      #:y-min y-min
      #:x-max x-max
      #:y-max y-max
      #:y-label #f
      #:x-label xlbl)))

(define (fn->title str)
  (big-text (path->string (file-name-from-path str))))

(define (big-text str)
  (text str '() 16))

(define (f:plot-clienttime-distro dd)
  ;; TODO color rectangles by mode
  ;; TODO legend = # in each mode
  ;; TODO something funny with @timestamp vs clientTimestamp, big gaps, sent mail to alan
  (define title* (first dd))
  (define row* (second dd))
  (define tm-idx (title-index title* key:ctime))
  (define r->d (row->datetime tm-idx))
  (define (getx r)
    (define d (r->d r))
    (define d% (datetime
                 (->year d)
                 (->month d)
                 (->day d)
                 (->hours d)
                 0 0 0))
    (->posix d%))
  (define count#
    (for/fold ((acc (hash)))
              ((rr (in-list row*)))
      (hash-add1 acc (getx rr))))
  (define hh (sort (hash->list count#) < #:key car))
  (void
    (with-output-to-file "out/row-distribution.rktd"
      #:exists 'replace
      (lambda ()
        (writeln hh))))
  #;
  (parameterize ([plot-x-far-ticks no-ticks]
                 [plot-y-far-ticks no-ticks]
                 [plot-x-ticks (time-dist-ticks)]
                 [plot-y-ticks (linear-major-y-ticks 3)]
                 [plot-font-size 8]
                 [plot-font-family 'roman]
                 [rectangle-color bg-dark-blue]
                 [rectangle-line-color bg-dark-blue]
                )
    (define xmin #f)
    (define xmax #f)
    (define my-bars
      (rectangles
        (for/list ((f+s (in-list hh)))
          (define log-sec (car f+s))
          (when (or (not xmin) (< log-sec xmin))
            (set! xmin log-sec))
          (when (or (not xmax) (< xmax log-sec))
            (set! xmax log-sec))
          (define num-row (cdr f+s))
          (vector (ivl log-sec (+ log-sec 1800))
                  (ivl 2 num-row)))))
    (void
      (set! xmin (datetime-floor (posix->datetime xmin)))
      (set! xmax (+days (datetime-floor (posix->datetime xmax)) 1)))
    (define ymin 0)
    (define ymax (apply max (map cdr hh)))
    (define weekend-shade
      (rectangles
        (for*/list ((ii (in-range (days-between xmin xmax)))
                    (dd (in-value (+days xmin ii)))
                    #:when (saturday? dd))
          (vector (ivl (->posix dd) (->posix (+days dd 2)))
                  (ivl ymin ymax)))
        #:color "red"
        #:line-style 'transparent
        #:line-color "red"
        #:alpha 0.2))
    (define-values [out-file out-kind]
      (let ((out-kind 'pdf))
        (values (format "out/row-distribution.~a" out-kind) out-kind)))
    (plot-file
      (list weekend-shade my-bars)
      out-file
      out-kind
      #:width  700 ; 300 ; 400  ; 160
      #:height 280 ; 100 ; 200  ;  60
      #:x-min (->posix xmin)
      #:x-max (->posix xmax)
      #:y-max ymax
      #:y-min ymin
      #:x-label #f #:y-label #f #:title #f)
    (printf "plot-file ~a~n" out-file)
    (void)))

(define ((f:plot-event-bars #:x-min [x-min #f] #:x-max [x-max #f] #:y-min [y-min #f] #:y-max [y-max #f]) filename lbl f-row*)
  ;; x = event index,
  ;; y = num logs at that index with TE < FS
  ;;  will see a power law, that's fine (can even out with %s)
  (define row** (file->value filename))
  (define bool**
    (for/list ((row* (in-list row**))
               #:unless (null? row*))
      (f-row* row*)))
  (define-values [true* false*]
    (let* ((len (apply max (map length bool**)))
           (tru* (make-vector len 0))
           (fal* (make-vector len 0))
           (++! (lambda (vec ii) (vector-set! vec ii (add1 (vector-ref vec ii))))))
      (for ((bool* (in-list bool**)))
        (for ((vv (in-list bool*))
              (ii (in-naturals))
              #:unless (and (number? vv) (zero? vv)))
          (++! (if vv tru* fal*) ii)))
      (values tru* fal*)))
  (parameterize ([plot-x-far-ticks no-ticks]
                 [plot-y-far-ticks no-ticks]
                 ;;[plot-x-ticks (time-dist-ticks)]
                 [plot-x-ticks (linear-major-y-ticks 4)]
                 [plot-y-ticks (linear-major-y-ticks 3)]
                 [plot-font-size 10]
                 [plot-font-family 'roman])
    (printf "plot-pict ~a~n" lbl)
    (define bar-width 1/4)
    (define bar-sep 1/8)
    (define my-bars
      (for/list ((count* (in-list (list true* false*)))
                 (cc (in-naturals 3))
                 (lbl (in-list '(#true #false))))
        (rectangles
          (for/list ((yy (in-vector count*))
                     (ii (in-naturals)))
            (define x1 (+ ii bar-width bar-sep))
            (vector
              (if lbl
                (ivl ii (+ ii bar-width))
                (ivl x1 (+ x1 bar-width)))
              (ivl 0 yy)))
          #:color cc
          #:alpha 0.9
          #:line-color cc
          #:label (~a lbl))))
    (plot-pict
      my-bars
      #:width 800
      #:height 300
      #:y-max y-max
      #:y-min y-min
      #:x-max x-max
      #:x-min x-min
      #:legend-anchor 'top-right
      #:y-label #f
      #:x-label #f #;lbl )))

(define (saturday? dd)
  (equal? (~t dd "E") "Sat"))

(define (weekend? dd)
  (member (~t dd "E") '("Sat" "Sun")))

(define (time-dist-ticks)
  (define (time-dist-layout ax-min ax-max)
    (define d0 (posix->datetime ax-min))
    (define d1 (posix->datetime ax-max))
    (cons
      (pre-tick (->posix d0) #true)
      (let loop ((acc (+days (datetime (->year d0) (->month d0) (->day d0) 0 0 0 0) 1)))
        (cond
          [(datetime<? acc d1)
           (cons (pre-tick (->posix acc) #true)
                 (loop (+days acc 1)))]
          [else
           (list (pre-tick (->posix d1) #true))]))))
  (define (time-dist-format ax-mix ax-max pt*)
    (cons
      (posix->iso (pre-tick-value (first pt*)))
      (snoc
        (posix->iso (pre-tick-value (last pt*)))
        (for/list ((_ (in-list (cddr pt*)))
                   (i (in-naturals)))
          (make-string i)))))
  (ticks time-dist-layout time-dist-format))

(define (linear-major-y-ticks num-ticks)
  (ticks
    (linear-major-layout num-ticks)
    linear-major-format))

(define ((linear-major-layout num-ticks) ax-min ax-max)
  (for/list ((ii (in-list (linear-seq ax-min ax-max num-ticks))))
    (pre-tick (exact-floor ii) #true)))

(define (linear-major-format ax-min ax-max pre-ticks)
  (map (compose1 number->string pre-tick-value) pre-ticks))

(define (string->number* str #:err [err #true])
  (define v (string->number (num-clean str)))
  (if (real? v)
    v
    (and err (raise-argument-error 'string->number* "(stringof real?)" str))))

(define (num-clean str)
  (let* ((str (string-replace str "," ""))
         (str (unstr str))
         (str (if (string=? str "-") "0" str)))
    str))

(define (unstr str)
  (let* ((L (string-length str))
         (ne (non-empty-string? str))
         (str (if (and ne (eq? #\" (string-ref str (- L 1)))) (substring str 0 (- L 1)) str))
         (str (if (and ne (eq? #\" (string-ref str 0))) (substring str 1) str)))
    str))

(define (pred->idx# title* ff)
  (define name*
    (for/list ((tt (in-list title*))
               #:when (ff tt))
      tt))
  (define idx#
    (for/hash ((name (in-list name*)))
      (values (string->number (cadr (regexp-match #rx"Error([0-9]+)In" name)))
              (title-index title* name))))
  idx#)

(define (te-survival title*)
  (define error-code* (hash-keys type-error#))
  (define (matches-a-code? str)
    (define pre-pat (string-append "\\." str "~aIn"))
    (lambda (tt)
      (for/or ((ecode (in-list error-code*)))
        (regexp-match? (format pre-pat ecode) tt))))
  (define curr-err-idx# (pred->idx# title* (matches-a-code? "TypeError")))
  (define olde-err-idx# (pred->idx# title* (matches-a-code? "OldTypeError")))
  (define ((mk-ref h#) ecode row)
    (let ((vv (hash-ref h# ecode #f)))
      (if vv
        (string->number* (values #;echo (list-ref row vv)))
        0)))
  (define curr-ref (mk-ref curr-err-idx#))
  (define olde-ref (mk-ref olde-err-idx#))
  (define row->modswitch? (compose1 string->bool (idx->getter title* key:switchedmodule)))
  ;; ---
  (define (my-fold acc row*)
    (for*/fold ((acc acc))
               ((row (in-list row*))
                #:unless (row->modswitch? row)
                (ecode (in-list error-code*)))
      (define num-curr (curr-ref ecode row))
      (define num-olde (olde-ref ecode row))
      (cond
       [(= 0 num-curr num-olde)
        acc]
       [else
        (hash-cons acc ecode (- num-curr num-olde))])))
  (define my-init (hash))
  (values my-fold my-init))

(define (csv->title* fname)
  (with-input-from-file
    fname
    (lambda ()
      (define title* (map unstr (comma-split (read-line))))
      title*)))

(define (merge-csv* csv*)
  (define title* (map csv->title* csv*))
  (define the-titles (reverse (apply set-intersect title*)))
  (void
    (for ((titles (in-list title*))
          (ii (in-naturals))
          #:unless (set=? the-titles titles))
      (log-luau-info "dropping columns from csv ~a: ~a"
        ii (set-subtract titles the-titles))))
  (define parsed*
    (for/list ((csv (in-list csv*)))
      (parse-data csv)))
  (define row**
    (for/list ((dd (in-list parsed*)))
      (define src-tt (first dd))
      (define tgt-tt the-titles)
      (define f-shuffle (make-rearranger src-tt tgt-tt))
      (for/list ((row (in-list (second dd))))
        (f-shuffle row))))
  (log-luau-info "merge rows, appending ~s files" (length row**))
  ;; TODO optimize for speed?
  (list the-titles (apply append row**)))

(define (make-rearranger src* tgt*)
  (define idx* (for/list ((tgt (in-list tgt*))) (index-of src* tgt)))
  (lambda (rr)
    (for/list ((src-idx (in-list idx*)))
      (list-ref rr src-idx))))

(define (parse-data fname #:clean? [clean? #true])
  (log-luau-info "parse-data: ~s" (path->string (file-name-from-path fname)))
  (with-input-from-file
    fname
    (lambda ()
      (define title* (map unstr (comma-split (read-line))))
      (define tm-idx (title-index title* key:ctime))
      (define r->idx (idx->getter title* key:sessionid))
      (define r->d (row->datetime tm-idx))
      (define num-col (length title*))
      (define (row->seen rr) ;; same session, same datetime
        (cons (r->idx rr) (r->d rr)))
      (define seen# (make-hash))
      (define stop-counter 0)
      (define row*
        (for*/list ((ln (in-lines))
                    (rr (in-value (parse-row ln num-col)))
                    (kk (in-value (row->seen rr)))
                    #:break (= stop-counter 70)
                    #:unless (let ((vv (hash-ref seen# kk #f)))
                               (and vv #;(printf "SEEN IT ~s~n" kk))))
          ;; (set! stop-counter (+ 1 stop-counter))
          ;; (log-luau-info "parse row ~a" stop-counter)
          (hash-set! seen# kk #true)
          rr))
      #;(printf "num rows = ~a~n" (length row*))
      (list title* row*))))

#;(define (dd->kv# dd)
  (define row* TODO)
  (define kv#
    (let ((H (make-hash)))
      (for ((rr (in-list row*)))
        (for ((vv (in-list rr))
              (tt (in-list title*)))
          (hash-update! H tt (lambda (x) (cons vv x)) (lambda () '()))))
      H))
  kv#)

(define (csv->dd-tt csv)
  (log-luau-info "parsing dataset ~s" csv)
  (define dd (parse-data csv))
  (define tt (timelines dd))
  (log-luau-info "done parsing")
  (values dd tt))

(define (len>1 tt)
  (and (not (null? tt)) (not (null? (cdr tt)))))

(define (comma-split str)
  (string-split str ","))

(define (parse-row str0 num-col)
  (define val*
    (let loop ((str str0))
      #;(printf "STR ~s~n" str)
      (cond
        [(not (non-empty-string? str))
         '()]
        [(eq? #\" (string-ref str 0))
         (define tgt (+ 1 (string-find str #\" 1)))
         (define L (string-length str))
         (cons (substring str 0 tgt) (loop (substring str (min L (+ 1 tgt)))))]
        [else
         (define tgt (string-find str #\,))
         (define L (string-length str))
         #;(printf "FIND ~s~n" tgt)
         (cons (substring str 0 tgt) (loop (substring str (min L (+ 1 tgt)))))])))
  (if (= (length val*) num-col)
    val*
    (raise-arguments-error 'parse-row "wrong number of columns" "expected" num-col "received" (length val*) "values" val*)))

(define (string-find str chr [start-idx 0])
  (define L (string-length str))
  (or (for/first ((i (in-naturals start-idx))
                  #:break (>= i L)
                  #:when (eq? (string-ref str i) chr))
                 i)
      L))

(define (make-row->posix tdx)
  (let ((rd (row->datetime tdx)))
    (lambda (row)
      (->posix (rd row)))))

(define row->posix make-row->posix)

(define ((row->datetime tdx) row)
  (define ts (list-ref row tdx))
  (parameterize ((current-timezone "America/Los_Angeles"))
    (ts->datetime ts)))

(define (ts->datetime str)
  ;; http://unicode.org/reports/tr35/tr35-dates.html#Date_Field_Symbol_Table
  (parse-datetime (unstr str) "MMM d, y @ HH:mm:ss.S"))

(define (timelines dd)
  (define title* (first dd))
  (define row* (second dd))
  (log-luau-info "building timelines for ~s rows" (length row*))
  (define idx (title-index title* key:sessionid))
  (define tdx (title-index title* "clientTimestamp"))
  (let* ((tt (group-by (lambda (x) (list-ref x idx)) row*))
         (tt (for/list ((timeline (in-list tt))) (sort timeline < #:key (row->posix tdx)))))
    (cons title* tt)))

(define (idx->getter title* key)
  (define idx (title-index title* key))
  (lambda (rr) (list-ref rr idx)))

(define (title-index title* str)
  (define idx
    (if (or (string-prefix? str "Studio-Luau-TypeErrors")
            (string=? str key:stime))
      (index-of title* str)
      (let ((suffix (string-append "." str)))
        (index-where title* (lambda (x) (string-suffix? x suffix))))))
  (if idx
    idx
    (raise-arguments-error 'title-index "no match" "str" str "title*" title*)))

(define (mode<? m0 m1)
  (< (mode->int m0) (mode->int m1)))

(define (mode->int m)
  (index-of roblox-mode* m))

(define fzero (lambda () 0))

(define (hash-++ h k n)
  (hash-update h k (lambda (m) (+ n m)) fzero))

(define (hash-++! h k n)
  (hash-update! h k (lambda (m) (+ n m)) fzero))

(define (hash-add1 h k)
  (hash-update h k add1 fzero))

(define (hash-add1! h k)
  (hash-update! h k add1 fzero))

(define (hash-cons h k v)
  (hash-update h k (lambda (xx) (cons v xx)) (lambda () '())))

(define (hash-cons! h k v)
  (hash-update! h k (lambda (xx) (cons v xx)) (lambda () '())))

(define (take-some xx)
  (define yy (take xx 2))
  (printf "take-some ~s~n" yy)
  yy)

(define (datetime-floor d)
  (datetime (->year d)
            (->month d)
            (->day d)
            0 0 0 0))

(define (posix->iso x)
  (define str (datetime->iso8601 (posix->datetime x)))
  (define y-t* (string-split str "T"))
  (car y-t*)
  #;(define s-ms (string-split (second y-t*) "."))
  #;(string-append (car y-t*) " " (car s-ms)))

(define (snoc x x*)
  (append x* (list x)))

(define (comma-join xx)
  (string-join xx ","))

(define (displaycsv x)
  (displayln (comma-join x)))

(define (echo x)
  (displayln x)
  x)

;; ---

(define-syntax-rule (with-outfile str e ...)
  (with-output-to-file #:exists 'replace str (lambda () e ...)))

(module+ main
  (require racket/cmdline)
  (define *mode (box 'main))
  (command-line
    #:once-each
    [("-m" "--mode") mf "mode: (or/c 'explore 'analyze)" (set-box! *mode (string->symbol mf))]
    #:args -data-csv*
    (define data-csv* ( #;take-some values
                        (if (not (null? -data-csv*)) -data-csv* (glob (build-path data-dir "*.csv")))))
    (define full-csv "out/full-dataset.csv")
    (define (full-data)
      (define dd
        (let ()
          (unless (file-exists? full-csv)
            (define dd (merge-csv* data-csv*))
            (with-outfile full-csv
              (displaycsv (first dd))
              (for-each displaycsv (second dd))
              (void)))
          (filter/ts-bounds (parse-data full-csv))))
      (define tt (timelines dd))
      (log-luau-info "done building full dataset")
      (values dd tt))
    (case (unbox *mode)
     ((main) ;; KEEP
      (define-values [dd tt] (full-data))
      (with-outfile "out/overview.txt"
        (t:dataset-overview dd tt)
        (t:count-te-region dd))
      (with-outfile "out/sessions.txt" (t:session-overview dd tt))
      (with-outfile "out/ctc-info.txt" (ctc-info dd))
      #;(f:plot-clienttime-distro dd)
      (void))
     ((modswitch) ;; KEEP
      (define modswitch-csv "out/modswitch.csv")
      (define (modswitch-data)
        (define dd
          (if (file-exists? modswitch-csv)
            (parse-data modswitch-csv)
            (let* ((dd (filter/ts-bounds (parse-data full-csv)))
                   (dd (filter/modswitch dd)))
              (with-outfile modswitch-csv (displaycsv (first dd)))
              dd)))
        (define tt (timelines dd))
        (values dd tt))
      (define-values [dd tt] (modswitch-data))
      (with-outfile "out/modswitch-overview.txt"
        (t:dataset-overview dd tt)
        (t:count-te-region dd)))
     ((ds divide-sessions) ;; KEEP
      (define-values [dd tt] (full-data))
      (define sg# (group-sessions dd tt))
      (for (((kk vv) (in-hash sg#)))
        (with-outfile (format "out/ss-~a.rktd" kk)
          (displayln "(")
          (for-each writeln vv)
          (displayln ")")))
      (void))
     ((query) ;; KEEP
      (unless (file-exists? full-csv)
        (raise-arguments-error 'query "build data file first" "missing file" full-csv))
      (define my-query-fn
        huge-files?
        #;huge-lines?
        #;te-action-at-distance?
        #;fs-action-at-distance?
        #;editsize-wrong?
        #;editsize-negative?
        #;ctc-session?)
      (define my-render-fn
        huge-lines-render
        #;te-aad-render
        #;fs-aad-render
        #;editsize-render
        #;negedit-render
        #;ctc-render)
      (query-stream full-csv my-query-fn my-render-fn))
     ((gap)
      (session-gaps (file->value "out/sessions.txt")))
     ((size-distro) ;; KEEP
      (define filename* '(
           "out/ss-strict.rktd"
           "out/ss-nonstrict.rktd"
           "out/ss-nocheck.rktd"
           "out/ss-hasup.rktd"
           "out/ss-hasdown.rktd"
           "out/ss-multimod.rktd"))
      (define size#
        (make-size-distribution filename*))
      (with-outfile "out/size-distributions.rktd" (pretty-write size#))
      (void))
     ((cte count-te-editrange)
      (define title* (csv->title* "out/full-dataset.csv"))
      (for ((fn (in-list '("out/ss-strict.rktd"
                           "out/ss-nonstrict.rktd"
                           "out/ss-nocheck.rktd"))))
        (parameterize ((pretty-print-columns 200))
          (with-outfile (format "out/te-editrange-~a" (path->string (file-name-from-path fn)))
            (t:count-te-region/session title* (file->value fn))))))
     ((ate aggregate-te)
      (define mode* '(strict nonstrict nocheck))
      (define (m->fn m) (format "out/error-density-ss-~a.rktd" m))
      (define row->te first)
      (define row->fs second)
      (for ((mm (in-list mode*)))
        (printf "~a~n" mm)
        (define vv* (file->value (m->fn mm)))
        (for ((sel (in-list (list row->te row->fs)))
              (nn (in-list '(te fs))))
          (define med# (make-hash))
          (define avg# (make-hash))
          (define n*
            (for*/list ((row* (in-list vv*))
                        (err* (in-value (map sel row*)))
                        ;#:when
                        ;  (void
                        ;    (hash-add1! med# (median < err*))
                        ;    (hash-add1! avg# (mean err*)))
                        (err (in-list err*)))
              err))
          (define mm (mean n*))
          (define std (stddev/mean mm n*))
          (define med (median < n*))
          (define xxx (apply max n*))
          (printf " ~a total ~a~n mean ~a std ~a med ~a max ~a~n med dist~n  ~a~n mean dist~n  ~a~n~n"
                  nn
                  (apply + n*)
                  mm std med xxx
                  '() '())
                  ;; med# avg#
          (void))))
     ((sq session-query)
      (define title* (csv->title* "out/full-dataset.csv"))
      (define my-query ((lambda (x) values)
                        #;huge-ctime?
                        title*))
      (define my-render
        #;(lambda (rr*) (map (oldecurr-render title*) rr*))
        (lambda (rr*) (map (error-density-render title*) rr*))
        #;huge-ctime-render)
      (define my-name
        "error-density"
        #;"huge-ctime")
      ;; TODO old + curr TS FS in module ... just do all
      (for ((fn (in-list '( "out/ss-strict.rktd"
                            #;"out/ss-multimod.rktd"
                            #;"out/ss-hasup.rktd"
                            #;"out/ss-hasdown.rktd"
                            "out/ss-nonstrict.rktd"
                            "out/ss-nocheck.rktd"))))
        (parameterize ((pretty-print-columns 200))
          (with-outfile (format "out/~a-~a" my-name (path->string (file-name-from-path fn)))
            (begin
              (printf ";; (list TE FS TE-MOD FS-MOD TE-EDIT FS-EDIT LINES EDIT SWITCH CTIME)~n")
              #;(printf ";; (list TE-RANGE FS-RANGE OLDE-TE-RANGE OLDE-FS-RANGE)~n")
              (pretty-write
                (for/list ((row* (in-list (file->value fn)))
                           #:when (my-query row*))
                  (my-render row*))))))
        (void)))
     ((sfold session-fold)
      (define title* (csv->title* "out/full-dataset.csv"))
      (define-values [my-fold my-init] (te-survival title*))
      (define my-name "type-error-survival")
      (for ((fn (in-list '("out/ss-strict.rktd"
                           "out/ss-nonstrict.rktd"
                           "out/ss-nocheck.rktd"))))
        (define vv (file->value fn))
        (define fold-res
          (for/fold ((acc my-init))
                    ((row* (in-list vv)))
            (my-fold acc row*)))
        (parameterize ((pretty-print-columns 200))
          (with-outfile (format "out/~a-~a" my-name (path->string (file-name-from-path fn)))
            (pretty-write fold-res)))))
     ((benc plot-explore)
      (define title* (csv->title* "out/full-dataset.csv"))
      (define pp (plot-explore title* '(
            "out/ss-strict.rktd"
            "out/ss-nonstrict.rktd"
            "out/ss-nocheck.rktd"

      ;    "out/ss-hasup.rktd"
      ;   "out/ss-hasdown.rktd"
      ;    "out/ss-multimod.rktd"
        )))
      (save-pict "plot-explore.png" pp))
     ((explore) (explore data-csv*))
     ((overview) (t:dataset-overview* data-csv*))
     (else (raise-argument-error 'main "(or/c 'explore 'analyze ....)" (unbox *mode))))
    (void)))

