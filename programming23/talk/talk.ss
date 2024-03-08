#lang at-exp slideshow

;; outline: https://docs.google.com/presentation/d/1Ci0SgJgme-7Vd8X-1GZ8n_lpYwdj_RtrxrpDIeL2Qts/edit
;; colors:  https://imagecolorpicker.com/
;; blog:    https://blog.brownplt.org/2024/02/02/privacy-telemetry.html

;; Programming Conference
;; March 2024
;; 30 min
;; https://2024.programming-conference.org/

;; [ ] main issue is that it ends all about concrete things about Luau, but the
;;     paper is actually about privacy-respecting telemetry. So I'd want to see two
;;     conclusions: one about Luau/whatever else you currently talk about, one more
;;     about setting the agenda for this kind of research going forward.
;; [X] roblox logo == brown size

;; Privacy-Respecting Type Error Telemetry at Scale
;; - language design, the problems, designers vs users
;;   user studies not great
;; - telemetry eminently useful
;;   api deprecation, goes from impossible to trivial
;;   MORE
;; - telemetry dangerous
;;   trade secrets
;;   
;; - roblex context
;;   luau, types
;;   they DO user studies
;; - RQs
;; - our design
;;   - general constraints:
;;   - specific constraints:
;; - more luau
;; - experiment
;;   - N months
;;   - K sessions etc.
;; - easy ... 3-4 graphs
;;   callouts callouts
;; - conclusions, answers to RQs
;; extra:
;; - cox transparent

(require
  racket/class
  racket/draw
  racket/format
  racket/match
  racket/list
  racket/string
  racket/runtime-path
  pict
  ppict/2
  pict-abbrevs
  gtp-pict
  (only-in images/icons/misc close-icon magnifying-glass-icon)
  (only-in images/icons/symbol check-icon)
  (only-in images/icons/control stop-icon)
  images/icons/style
  ppict/pict ppict/tag
  pict-abbrevs/slideshow
  plot/no-gui (except-in plot/utils min* max*))

(define-runtime-path img-dir "./src")
(define src-dir img-dir)

(define turn revolution)

(define *export* (make-parameter #false))

(define x%->pixels w%->pixels)
(define y%->pixels h%->pixels)

(define pico-x-sep (w%->pixels 1/100))
(define tiny-x-sep (w%->pixels 2/100))
(define border-x-sep (w%->pixels 4/100))
(define small-x-sep (w%->pixels 5/100))
(define smol-x-sep small-x-sep)
(define med-x-sep (w%->pixels 10/100))
(define big-x-sep (w%->pixels 15/100))
(define medd-x-sep med-x-sep)
(define bigg-x-sep big-x-sep)

(define pico-y-sep (h%->pixels 1/100))
(define tiny-y-sep (h%->pixels 2/100))
(define small-y-sep (h%->pixels 5/100))
(define smol-y-sep small-y-sep)
(define med-y-sep (h%->pixels 10/100))
(define big-y-sep (h%->pixels 15/100))
(define medd-y-sep med-y-sep)
(define bigg-y-sep big-y-sep)

(define slide-top 4/100)
(define slide-left 2/100)
(define slide-right (- 1 slide-left))
(define slide-bottom 82/100)
(define slide-text-left (* 3 slide-left)) ;; 3/2 SD 4:3
(define head-left 20/100) ;; slide-left SD 4:3
(define head-right (- 1 head-left)) ;; slide-right SD 4:3
(define text-left slide-text-left)
(define slide-text-right (- 1 slide-text-left))
(define text-right slide-text-right)
(define slide-heading-top (* 1.4 slide-top))
(define slide-text-top (* 4 slide-top))
(define hi-text (* 6 slide-top))
(define lo-text (* 2.5 hi-text))
(define slide-text-bottom slide-bottom)
(define lesson-x 18/100)

(define slide-text-coord (coord slide-text-left slide-text-top 'lt))
(define slide-text-coord-left slide-text-coord)
(define slide-text-coord-mid (coord 1/2 slide-text-top 'ct))
(define slide-text-coord-right (coord slide-text-right slide-text-top 'rt))
(define slide-text-coord-l  slide-text-coord-left)
(define slide-text-coord-m   slide-text-coord-mid)
(define slide-text-coord-r slide-text-coord-right)
(define heading-text-coord (coord head-left slide-heading-top 'lt))
(define heading-text-coord-left heading-text-coord)
(define heading-text-coord-mid (coord 1/2 slide-heading-top 'ct))
(define heading-text-coord-right (coord head-right slide-heading-top 'rt))
(define heading-coord heading-text-coord)
(define heading-coord-left heading-text-coord-left)
(define heading-coord-mid heading-text-coord-mid)
(define heading-coord-right heading-text-coord-right)
(define heading-coord-l  heading-coord-left)
(define heading-coord-m  heading-coord-mid)
(define heading-coord-r  heading-coord-right)
(define bottom-coord-left (coord slide-left slide-text-bottom 'lb))
(define bottom-coord-mid (coord 1/2 slide-text-bottom 'cb))
(define bottom-coord-right (coord slide-right slide-text-bottom 'rb))
(define bottom-coord-l bottom-coord-left)
(define bottom-coord-m bottom-coord-mid)
(define bottom-coord-r bottom-coord-right)
(define center-coord (coord 1/2 1/2 'cc))
(define title-coord (coord 1/2 26/100 'ct))
(define hi-text-coord-left (coord slide-text-left hi-text 'lt))
(define hi-text-coord-mid (coord 1/2 hi-text 'ct))
(define hi-text-coord-right (coord slide-text-right hi-text 'rt))
(define hi-text-coord-l hi-text-coord-left)
(define hi-text-coord-m   hi-text-coord-mid)
(define hi-text-coord-r hi-text-coord-right)
(define hi-text-coord-ll  (coord 48/100 hi-text 'rt))
(define hi-text-coord-rr (coord 52/100 hi-text 'lt))
(define lo-text-coord-left (coord slide-text-left lo-text 'lt))
(define lo-text-coord-mid (coord 1/2 lo-text 'ct))
(define lo-text-coord-right (coord slide-text-right lo-text 'rt))
(define all-lang-coord (coord 99/100 1/2 'rc))
(define lesson-coord-h (coord lesson-x hi-text  'lt))
(define lesson-coord-m (coord lesson-x (+ 15/100 hi-text) 'lt))
(define lesson-coord-l (coord lesson-x (+ 30/100 hi-text) 'lt))
(define title-coord-m (coord 1/2 23/100 'ct))

(define default-line-width 4)
(define default-arrow-size 14)
(define large-arrow-size 18)

(define code-brush-alpha 0.6)

(define (color%++ c n)
  (make-object color%
               (byte-round (+ (send c red) n))
               (byte-round (+ (send c green) n))
               (byte-round (+ (send c blue) n))
               (send c alpha)))

(define (byte-round n)
  (if (< n 0)
    0
    (if (< 255 n)
      255 n)))

(define at-sign @"@")

(define roblox-black (hex-triplet->color% #x0e0f0f))
(define black roblox-black)
(define gray (string->color% "light gray"))
(define white (string->color% "white"))
(define lite-grey (hex-triplet->color% #xeeeeee)) ; "gainsboro"
(define transparent (color%-update-alpha white 0))
(define dark-orange (hex-triplet->color% #xE05626))
(define lite-orange (hex-triplet->color% #xF89C3F))
(define dark-blue (hex-triplet->color% #x002E6D))
(define bg-dark-blue (hex-triplet->color% #x2C6B91))
(define bg-lite-blue (hex-triplet->color% #x357C9F))
(define lite-blue (hex-triplet->color% #xC0EFFF))
(define lite-green (hex-triplet->color% #x00b18f))

(define roblox-litered (hex-triplet->color% #xe5001d))
(define roblox-darkred (hex-triplet->color% #xa90e1c))
(define utah-red (hex-triplet->color% #xCC0000))
(define utah-black (hex-triplet->color% #x000000))
(define utah-white (hex-triplet->color% #xFFFFFF))
(define utah-sunrise (hex-triplet->color% #xFFB81D))
(define utah-lake (hex-triplet->color% #x3ABFC0))
(define utah-crimson (hex-triplet->color% #x890000))
(define utah-granite (hex-triplet->color% #x708E99))
(define utah-darkgrey (hex-triplet->color% #xE2E6E6))
(define utah-litegrey (hex-triplet->color% #xF7F9FB))

(define typed-color utah-sunrise)
(define untyped-color utah-granite)
(define shallow-color utah-lake)
(define concrete-color (hex-triplet->color% #xB17b03 #;F5ac0a))
(define primitive-color utah-lake)
(define deep-color typed-color)
(define typed-brush-color (color%++ typed-color 20))
(define shallow-pen-color shallow-color #;(hex-triplet->color% #xffc20a) )
(define deep-pen-color deep-color #;(hex-triplet->color% #x0c7bdc))
(define concrete-pen-color concrete-color)
(define primitive-pen-color primitive-color)
(define untyped-pen-color untyped-color)
(define shallow-brush-color (color%-update-alpha shallow-pen-color 0.4) #;lite-orange #;(hex-triplet->color% #xfdc008))
(define deep-brush-color (color%-update-alpha deep-pen-color 0.4) #;(hex-triplet->color% #x0a79da))
(define concrete-brush-color (color%-update-alpha concrete-pen-color 0.4) #;(hex-triplet->color% #x0a79da))
(define primitive-brush-color (color%-update-alpha primitive-pen-color 0.4) #;(hex-triplet->color% #x0a79da))
(define untyped-brush-color (color%-update-alpha untyped-pen-color 0.4) #;(color%++ untyped-color 20))
(define fog-3k1 (hex-triplet->color% #xDBCAC2))
(define neutral-brush-color fog-3k1)
(define green0-3k1 (hex-triplet->color% #x71BE8D))
(define green1-3k1 (hex-triplet->color% #x598F61))
(define green2-3k1 (hex-triplet->color% #x4F7459))
(define red0-3k1 (hex-triplet->color% #xF0749C))
(define red1-3k1 (hex-triplet->color% #xC3476F))
(define apple-green lite-green)
(define apple-red red1-3k1)
(define typed-pen-color #f)
(define validate-pen-color red1-3k1)
(define validate-brush-color (color%-update-alpha validate-pen-color code-brush-alpha))
(define happy-cloud-color lite-blue)
(define sad-cloud-color dark-blue)
(define default-line-color dark-blue)
(define hilite-frame-color dark-orange)
(define blame-color typed-color)
(define shallow-bg-color (color%-update-alpha shallow-pen-color 0.2))
(define deep-bg-color  (color%-update-alpha deep-pen-color 0.2))
(define typed-bg-color deep-bg-color)
(define untyped-bg-color (color%-update-alpha untyped-pen-color 0.2))

(define emph-color roblox-darkred #;(hex-triplet->color% #x304E59))
(define bg-color (hex-triplet->color% #xcecfcf))

(define bbox-frame-color (make-parameter dark-blue))
(define bbox-radius (make-parameter 1))
(define bbox-x-margin (make-parameter small-x-sep))
(define bbox-y-margin (make-parameter tiny-y-sep))
(define bbox-frame-width (make-parameter 2))

(define (color-off c)
  (color%-update-alpha c 0.2))

(define title-font "Gotham")
(define body-font "Gotham")
(define code-font "Inconsolata")

(define title-size 42)
(define subtitle-size 32)
(define head-size 38)
(define body-size 40)
(define code-size 32)
(define tcode-size (- code-size 4))

(define ((make-string->text #:font font #:size size #:color color) . str*)
  (colorize (text (apply string-append str*) font size) color))

(define (bold-style font)
  (cons 'bold font))

(define (italic-style font)
  (cons 'italic font))

(define body-font-lo (make-object font% body-size body-font 'default 'normal 'light))
(define body-font-it (make-object font% body-size body-font 'default 'italic 'light))
(define body-font-itbf (make-object font% body-size body-font 'default 'italic 'semibold))
(define body-font-md (make-object font% body-size body-font 'default 'normal 'medium))
(define body-font-hi (make-object font% body-size body-font 'default 'normal 'semibold))
(define utah-web-headline-font (make-object font% title-size title-font 'default 'normal 'semibold))
(define page-font (make-font #:face code-font #:size code-size))

(define titlerm (make-string->text #:font utah-web-headline-font #:size title-size #:color black))
(define subtitlerm (compose1 (lambda (pp) (scale pp 0.85)) titlerm))

(define titlerm2 (make-string->text #:font utah-web-headline-font #:size (- title-size 8) #:color black))
(define subtitlermem (make-string->text #:font (bold-style title-font) #:size subtitle-size #:color emph-color))
(define subtitlermemlo (make-string->text #:font title-font #:size subtitle-size #:color emph-color))
(define subtitlermlo
  (let ((ff (make-string->text #:font title-font #:size subtitle-size #:color black)))
    (lambda str*
      (cellophane (apply ff str*) 0.7))))
(define headrm (make-string->text #:font title-font #:size head-size #:color dark-blue))
(define coderm (make-string->text #:font code-font #:size code-size #:color black))
(define codebf (make-string->text #:font (bold-style code-font) #:size code-size #:color black))
(define codeemrm (make-string->text #:font (bold-style code-font) #:size code-size #:color green2-3k1))
(define greenrm codeemrm)
(define codeemrm2 (make-string->text #:font (bold-style code-font) #:size code-size #:color emph-color))
(define codeembf (make-string->text #:font (bold-style code-font) #:size code-size #:color apple-red))
(define redrm codeembf)
(define tcoderm (make-string->text #:font code-font #:size tcode-size #:color black))
(define tcodebf (make-string->text #:font (bold-style code-font) #:size tcode-size #:color black))
(define tt coderm)

(define (shimhack f)
  (let ((tshim (yblank 32))
        (bshim (yblank 6)))
    (lambda str*
      (let ((pp (apply f str*)))
        (vl-append tshim pp bshim)))))

(define bodyrmhi (make-string->text #:font body-font-md #:size body-size #:color black))
(define hugerm (make-string->text #:font body-font-md #:size (+ 20 body-size) #:color black))
(define rmlo (shimhack (make-string->text #:font body-font-lo #:size body-size #:color black)))
(define rmem (shimhack (make-string->text #:font body-font-lo #:size body-size #:color emph-color)))
(define rmhi rmem)
(define bodyrmlobb (make-string->text #:font body-font-lo #:size body-size #:color deep-pen-color))
(define bodyrmloyy (make-string->text #:font body-font-lo #:size body-size #:color shallow-pen-color))
;; (define bodyrmhi (make-string->text #:font body-font-hi #:size body-size #:color black))
(define bodyrmhibb (make-string->text #:font body-font-hi #:size body-size #:color deep-pen-color))
(define bodyrmhiyy (make-string->text #:font body-font-hi #:size body-size #:color shallow-pen-color))
(define bodyit (make-string->text #:font body-font-it #:size body-size #:color black))
(define bodyitbf (make-string->text #:font body-font-itbf #:size body-size #:color black))
(define bodybf (make-string->text #:font (bold-style body-font) #:size body-size #:color black))
(define bodyemit (make-string->text #:font body-font-it #:size body-size #:color emph-color))
(define bodyemrm (make-string->text #:font body-font-md #:size body-size #:color emph-color))
(define bodyrmem bodyemrm)
(define bodyembf (make-string->text #:font (bold-style body-font) #:size body-size #:color emph-color))
(define bodyemrm2 (make-string->text #:font body-font-md #:size body-size #:color green2-3k1))
(define bodyembf2 (make-string->text #:font (bold-style body-font-md) #:size body-size #:color green2-3k1))
(define bodyembf3 (make-string->text #:font (bold-style body-font-md) #:size body-size #:color apple-red))
(define bodyemty (make-string->text #:font body-font-md #:size body-size #:color deep-pen-color))
(define bodyemun (make-string->text #:font body-font-md #:size body-size #:color untyped-color))
(define bodyembl (make-string->text #:font body-font-md #:size body-size #:color blame-color))

(define (at-find-right tag)
  (at-find-pict tag rc-find 'lc #:abs-x pico-x-sep))

(define (arrowhead-pict rad #:color [color black] #:size [size 20])
  (colorize
    (arrowhead size rad)
    color))

(define up-arrow-pict
  (arrowhead-pict (* 1/4 turn) #:color black))

(define right-arrow-pict
  (arrowhead-pict (* 0 turn) #:color black))

(define left-arrow-pict
  (arrowhead-pict (* 1/2 turn) #:color black))

(define down-arrow-pict
  (arrowhead-pict (* 3/4 turn) #:color black))

(define (sky-arrow)
  (define rr (* 1/2 turn))
  (define ss 20)
  (cc-superimpose
    (arrowhead-pict rr #:color (bbox-frame-color) #:size ss)
    (arrowhead-pict rr #:color white #:size (- ss 3))))

(define (author-append . pp*)
  (apply vl-append pico-y-sep pp*))

(define main-logo-w 200)
(define main-logo-h 100)

(define (-bitmap str)
  (define ps
    (if (and (string? str)
             (or (string-prefix? str "img/")
                 (string-prefix? str "src/")))
      (build-path img-dir (substring str 4))
      (if (string? str)
        (build-path img-dir str)
        str)))
  (bitmap ps))

(define (main-logo str [ww main-logo-w] [hh main-logo-h])
  (freeze (scale-to-fit (-bitmap str) (* 2 ww) hh)))

(define (brown-logo)
  (main-logo "browncs-logo.png"))

(define (cra-logo)
  (main-logo "cra-logo.png"))

(define (roblox-logo)
  (main-logo "roblox-logo.png"))

(define (roblox-logo-smol)
  (main-logo "roblox-logo.png" 170 90))

(define (luau-logo [ww #f])
  (freeze (scale-to-square (-bitmap "luau-logo.png") (or ww 260))))

(define (luau-logo-big)
  (luau-logo))

(define (luau-logo-smol)
  (luau-logo 150))

(define checker-w 40)

(define (make-checker c)
  (filled-rectangle checker-w checker-w #:draw-border? #f #:color c))

(define (make-checkerboard w h c0 c1)
  (let* ((b0 (make-checker c0))
         (b1 (make-checker c1))
         (b01 (ht-append b0 b1))
         (b10 (ht-append b1 b0))
         (make-row (lambda (pp) (apply ht-append (make-list (+ 1 (quotient (exact-ceiling w) (pict-width pp))) pp))))
         (row (vl-append (make-row b01) (make-row b10))))
    (apply vl-append (make-list (+ 1 (quotient (exact-ceiling h) (pict-height row))) row))))

(define ((slide-assembler/background2 base-assembler make-rect) slide-title slide-vspace slide-pict)
  (define foreground-pict (base-assembler slide-title slide-vspace slide-pict))
  (define background-pict
    (let ((+margin (* 2 margin))
          (-margin (- margin)))
      (inset (make-rect (+ +margin client-w) (+ +margin client-h)) -margin)))
  (cc-superimpose background-pict foreground-pict))

(define (make-solid-bg w h color)
  (let* ((bg (filled-rectangle w h #:color white #:draw-border? #f))
         (fg (filled-rectangle w h #:color color #:draw-border? #f)))
    (cc-superimpose bg fg)))

(define (make-bg w h) (make-solid-bg w h bg-color))

(define bg-orig (current-slide-assembler))
(define bg-bg (slide-assembler/background2 bg-orig make-bg))

(define (bbox pp
              #:color [color white]
              #:x-margin [x-margin #f]
              #:y-margin [y-margin #f]
              #:frame-color [frame-color #f]
              #:frame-width [frame-width #f]
              #:backup? [backup? #f])
  (define xm (or x-margin (bbox-x-margin)))
  (define ym (or y-margin (bbox-y-margin)))
  (define rr (bbox-radius))
  (add-rounded-border
    (if backup?
      (add-rounded-border
        pp
        #:x-margin xm #:y-margin ym #:radius rr
        #:background-color color #:frame-width 0)
      pp)
    #:x-margin (if backup? 0 xm)
    #:y-margin (if backup? 0 ym)
    #:radius rr
    #:background-color (if backup? white color)
    #:frame-width (or frame-width (bbox-frame-width))
    #:frame-color (or frame-color (bbox-frame-color))))

(define (thinkbox pp)
  (parameterize ((bbox-radius 12))
    (bbox pp #:x-margin pico-x-sep)))

(define (sbox pp)
  (bbox pp
        #:x-margin pico-y-sep
        #:y-margin pico-y-sep))

(define (sboxrm . arg*)
  (sbox (apply rmlo arg*)))

(define (wbox pp #:frame-color [frame-color #f] #:frame-width [frame-width #f])
  (bbox pp
        #:x-margin pico-x-sep
        #:y-margin pico-y-sep
        #:frame-color frame-color
        #:frame-width frame-width))

(define (wboxrm . arg*)
  (wbox (apply rmlo arg*)))

(define (bboxrm . arg*)
  (bbox (apply rmlo arg*)))

(struct code-arrow (src-tag src-find tgt-tag tgt-find start-angle end-angle start-pull end-pull style) #:transparent)

(define (add-code-arrow pp arrow
                        #:both [both-arrow #f]
                        #:arrow-size [pre-arrow-size #f]
                        #:line-width [pre-line-width #f]
                        #:color [color #f]
                        #:label [label (blank)]
                        #:x-adjust-label [x-label 0]
                        #:y-adjust-label [y-label 0]
                        #:hide? [hide? #false])
  (define line-width (or pre-line-width default-line-width))
  (define arrow-size (or pre-arrow-size default-arrow-size))
  ((if both-arrow pin-arrows-line pin-arrow-line)
    arrow-size pp
    (let ((src-tag (code-arrow-src-tag arrow)))
      (if (symbol? src-tag) (find-tag pp src-tag) src-tag))
    (code-arrow-src-find arrow)
    (let ((tgt-tag (code-arrow-tgt-tag arrow)))
      (if (symbol? tgt-tag) (find-tag pp tgt-tag) tgt-tag))
    (code-arrow-tgt-find arrow)
    #:line-width line-width
    #:label label
    #:x-adjust-label x-label
    #:y-adjust-label y-label
    #:hide-arrowhead? hide?
    #:style (code-arrow-style arrow)
    #:start-angle (code-arrow-start-angle arrow)
    #:end-angle (code-arrow-end-angle arrow)
    #:start-pull (code-arrow-start-pull arrow)
    #:end-pull (code-arrow-end-pull arrow)
    #:color (or color default-line-color)))

(define (add-code-line pp arrow
                       #:line-width [pre-line-width #f]
                       #:color [color default-line-color]
                       #:label [label (blank)]
                       #:x-adjust-label [x-label 0]
                       #:y-adjust-label [y-label 0]
                       #:hide? [hide? #false])
  (add-code-arrow pp arrow #:arrow-size 0
                  #:line-width pre-line-width #:color color #:label label
                  #:x-adjust-label x-label #:y-adjust-label y-label #:hide? hide?))

(define (add-code-arrows pp #:arrow-size [arrow-size #f] #:color [color #f] . arrow*)
  (add-code-arrows* pp arrow* #:arrow-size arrow-size #:color color))

(define (add-code-arrows* pp* arrow* #:color [color #f] #:arrow-size [arrow-size #f])
  (for/fold ((pp pp*))
            ((arrow (in-list arrow*)))
    (add-code-arrow pp arrow #:color color #:arrow-size arrow-size)))

(define add-code-arrow* add-code-arrows*)

(define (add-code-lines pp #:color [color #f] . arrow*)
  (add-code-line* pp arrow* #:color color))

(define (add-code-line* pp arrow* #:color [color #f])
  (for/fold ((pp pp))
            ((arrow (in-list arrow*)))
    (add-code-line pp arrow #:color color)))

(define (ben-rule w h #:color [color #f])
  (filled-rectangle w h #:color (or color dark-blue) #:draw-border? #f))

(define (bvrule h #:thickness [thickness #f] #:color [color #f])
  (ben-rule (or thickness 1) h #:color color))

(define (bhrule w #:thickness [thickness #f] #:color [color #f])
  (ben-rule w (or thickness 1) #:color color))

(define (plus-one [ww 40])
  (scale-to-square (-bitmap "plus-one.png") ww))

(define (scale-comment pp)
  (scale pp 0.65))

(define comment-scale scale-comment)

(define (scale-to-pict pp bg)
  (scale-to-fit pp (pict-width bg) (pict-height bg)))

(define (scale-within pp ww hh)
  (if (and (<= (pict-width pp) ww)
           (<= (pict-height pp) hh))
    pp
    (scale-to-fit pp ww hh)))

(define (add-lang str)
  (string-append "lang/" str))

(define (add-src str)
  (string-append "img/" str))

(define add-img add-src)

(define word-sep 0)

(define (word-append . pp*)
  (apply hb-append word-sep pp*))

(define line-sep2 (+ 10))

(define (left-line-append2 . pp*)
  (left-line-append2* pp*))

(define (left-line-append2* pp*)
  (apply vl-append line-sep2 pp*))

(define (mid-line-append2 . pp*)
  (mid-line-append2* pp*))

(define (mid-line-append2* pp*)
  (apply vc-append line-sep2 pp*))

(define (right-line-append2 . pp*)
  (right-line-append2* pp*))

(define (right-line-append2* pp*)
  (apply vr-append line-sep2 pp*))

(define ll-append left-line-append2)
(define lc-append mid-line-append2)
(define lr-append right-line-append2)

(define line-sep tiny-y-sep)

(define (left-line-append #:sep [sep #f] . pp*)
  (left-line-append* #:sep sep pp*))

(define l-line-append left-line-append)

(define (left-line-append* #:sep [sep #f] pp*)
  (apply vl-append (or sep line-sep) pp*))

(define (mid-line-append #:sep [sep #f] . pp*)
  (apply vc-append (or sep line-sep) pp*))

(define m-line-append mid-line-append)

(define (right-line-append . pp*)
  (apply vr-append line-sep pp*))

(define r-line-append right-line-append)

(define code-line-sep (h%->pixels 12/1000))

(define (code-line-append . pp*)
  (code-line-append* pp*))

(define (code-line-append* pp*)
  (apply vl-append code-line-sep pp*))

(define (codeblock-append #:sep [sep #f] . pp*)
  (codeblock-append* pp*))

(define (codeblock-append* #:sep [sep #f] pp*)
  (apply vl-append (or sep tiny-y-sep) pp*))

(define (hcodeblock-append #:sep [sep #f] . pp*)
  (hcodeblock-append* #:sep sep pp*))

(define (hcodeblock-append* #:sep [sep #f] pp*)
  (apply ht-append (or sep tiny-x-sep) pp*))

(define (scale-lang-lo pp)
  (scale-to-fit pp 120 80))

(define (lang-lo str)
  (scale-lang-lo (-bitmap str)))

(define (split/n lang-img* n)
  (let loop ((pp* lang-img*))
    (if (< (length pp*) n)
      (list pp*)
      (let-values (((a b) (split-at pp* n)))
        (cons a (loop b))))))

(define (X-codeblock pp* #:dark? [dark? #f] #:title [title #f] #:label [label #f] #:frame-color [frame-color #f] #:background-color [background-color #f])
  (define title-pict (if (pict? title) title (if (string? title) (rmlo title) #f)))
  (define label-margin (if title-pict (* 10/100 (pict-height title-pict)) 0))
  (define (add-label-margin pp [extra 0]) (vl-append (+ extra label-margin) (blank) pp))
  (define radius 1)
  (define fw 5)
  (let* ((block-pict
           (bbox
             (code-line-append* pp*)
             #:backup? #t
             #:frame-color #f #;(if dark? #f background-color)
             #:color (if dark?
                       background-color
                       (color%-update-alpha background-color 0.4)))))
    (if label
      (let ((block-pict (add-label-margin block-pict 2)))
        (ppict-do (if title-pict (lt-superimpose block-pict (ht-append 4 (blank) title-pict)) block-pict)
          #:go (coord 1/2 0 'ct) label))
      (if title-pict (vc-append 0 (ht-append 4 (blank) title-pict) (add-label-margin block-pict)) block-pict))))

(define (untyped-code str)
  (untyped-codeblock #:title #f #:lang #f str))

(define (untyped-codeblock #:dark? [dark? #f] #:title [title #f] #:lang [lang #f #;"#lang untyped"] . str*)
  (untyped-codeblock* #:dark? dark? #:title title str*))

(define (untyped-codeblock* pp* #:dark? [dark? #f] #:title [title #f])
  (X-codeblock pp* #:dark? dark? #:title title #:frame-color untyped-pen-color #:background-color untyped-brush-color))

(define (white-codeblock #:dark? [dark? #f] #:title [title #f] #:lang [lang #f #;"#lang untyped"] . str*)
  (white-codeblock* #:dark? dark? #:title title str*))

(define (white-codeblock* pp* #:dark? [dark? #f] #:title [title #f])
  (X-codeblock pp* #:dark? dark? #:title title #:frame-color untyped-pen-color #:background-color white))

(define (shallow-code str)
  (shallow-codeblock #:title #f #:lang #f str))

(define (shallow-codeblock #:dark? [dark? #f] #:title [title #f] #:lang [lang #f #;"#lang shallow"] . str*)
  (shallow-codeblock* #:dark? dark? #:title title str*))

(define (shallow-codeblock* pp* #:dark? [dark? #f] #:title [title #f])
  (X-codeblock pp* #:dark? dark? #:title title #:frame-color shallow-pen-color #:background-color shallow-brush-color))

(define (deep-code str)
  (deep-codeblock #:title #f #:lang #f str))

(define (deep-codeblock #:dark? [dark? #f] #:title [title #f] #:lang [lang #f #;"#lang deep"] . str*)
  (deep-codeblock* #:dark? dark? #:title title str*))

(define (deep-codeblock* pp* #:dark? [dark? #f] #:title [title #f])
  (X-codeblock pp* #:dark? dark? #:title title #:frame-color deep-pen-color #:background-color deep-brush-color))

(define (concrete-codeblock #:dark? [dark? #f] #:title [title #f] #:lang [lang #f #;"#lang concrete"] . str*)
  (concrete-codeblock* #:dark? dark? #:title title str*))

(define (concrete-codeblock* pp* #:dark? [dark? #f] #:title [title #f])
  (X-codeblock pp* #:dark? dark? #:title title #:frame-color deep-pen-color #:background-color concrete-brush-color))

(define typed-codeblock* deep-codeblock*)
(define nocheck-codeblock shallow-codeblock)
(define nocheck-codeblock* shallow-codeblock*)
(define nonstrict-codeblock deep-codeblock)
(define nonstrict-codeblock* deep-codeblock*)
(define strict-codeblock concrete-codeblock)
(define strict-codeblock* concrete-codeblock*)

(define (ucode str)
  (untyped-codeblock* (list (coderm str))))

(define (tcode str)
  (typed-codeblock* (list (coderm str))))

(define (ccode str)
  (concrete-codeblock* (list (coderm str))))

(define (untyped-box pp)
  (bbox #:x-margin 0 #:y-margin 0 #:color untyped-brush-color pp))

(define (typed-box pp)
  (bbox #:x-margin 0 #:y-margin 0 #:color deep-brush-color pp))

(define (typed-codeblock #:dark? [dark? #f] #:title [title #f] #:lang [lang #f #;"#lang typed"] . str*)
  (deep-codeblock* #:dark? dark? #:title title str*))

(define (pblank pp)
  (blank (pict-width pp) (pict-height pp)))

(define (bblur pp #:alpha [alpha #f] #:bg [bg? #f])
  (define fg (cellophane pp (or alpha 4/10)))
  (if bg?
    (cc-superimpose (bgrect fg) fg)
    fg))

(define (bblur2 pp)
  (bblur pp #:alpha 0.7))

(define (maybe-bblur yes? pp)
  (if yes?  (bblur pp) pp))

(define (bgrect pp)
  (brect pp bg-color))

(define (brect pp cc)
  (filled-rectangle (pict-width pp) (pict-height pp) #:draw-border? #f #:color cc))

(define xsep xblank)
(define ysep yblank)

(define (bghost pp)
  (blank (pict-width pp) (pict-height pp)))

(define big-swatch-blank (blank small-y-sep small-y-sep))

(define (untyped-icon-tiny)
  (parameterize ((bbox-x-margin 2) (bbox-y-margin 2))
    (untyped-codeblock* (list (blank 40 40)))))

(define (deep-icon-tiny)
  (parameterize ((bbox-x-margin 2) (bbox-y-margin 2))
    (deep-codeblock* (list (blank 40 40)))))

(define (shallow-icon-tiny)
  (parameterize ((bbox-x-margin 2) (bbox-y-margin 2))
    (shallow-codeblock* (list (blank 40 40)))))

(define (untyped-icon #:lbl [lbl "U"])
  (center-label
    (untyped-codeblock* #:title #f (list big-swatch-blank))
    lbl))

(define (typed-icon #:lbl [lbl "T"])
  (center-label
    (deep-codeblock* #:title #f (list big-swatch-blank))
    lbl))

(define (center-label pp lbl)
  (ppict-do
    pp
    #:go (coord 1/2 46/100 'cc)
    (if lbl (scale (headrm lbl) 0.9) (blank))))

(define (racket-pict hh)
  (freeze (scale-to-square (bitmap (build-path src-dir "racket.png")) hh)))

(define (label-below base . pp*)
  (vc-append 0 base (apply vc-append 2 pp*)))

(define (label-above base . pp*)
  (vc-append 0 (apply vc-append 2 pp*) base))

(define (mkchess n)
  (format "img/chess~a.png" n))

(define ((scale-square n) pp)
  (scale-to-square pp n))

(define (mag-icon [hh 60])
  (bitmap (magnifying-glass-icon #:height hh)))

(define (check-mini)
  (check-pict 40))

(define (check-mini2)
  (check-pict2 40))

(define (stop-mini)
  (stop-pict 30))

(define (stop-mini2)
  (stop-pict2 30))

(define (check-pict2 h)
  (bitmap (check-icon #:height h #:material metal-icon-material)))

(define (check-pict h)
  (bitmap (check-icon #:color apple-green #:height h #:material rubber-icon-material)))

(define (caution-pict h)
  (bitmap (close-icon #:color utah-sunrise #:height h #:material plastic-icon-material)))

(define (stop-pict h)
  (bitmap (stop-icon #:color utah-crimson #:height h #:material plastic-icon-material)))

(define (stop-pict2 h)
  (bitmap (stop-icon #:color utah-crimson #:height h #:material metal-icon-material)))

(define (shuffle-grid pp)
  (define row* (make-list 3 pp))
  (define sep 4)
  (apply
    vc-append
    sep
    (make-list 3 (apply hc-append sep row*))))

(define (rnd n)
  (~r n #:precision '(= 2)))

;; ---

(define (designers-and-users [n 0])
  (define lo (bbox @rmlo{Language Designers  &  Users need to talk!}))
  (define hi (penguin-row n))
  (vc-append tiny-y-sep hi ((if (= n 0) values pblank) lo)))

(define (penguin-row n)
    (let* ((hh 160)
           (maker (tag-pict (freeze (scale-to-height (-bitmap "penguin-wizard.png") hh)) 'maker))
           (users (tag-pict (freeze (scale-to-height (values (inset/clip (-bitmap "penguin-group.png") 0 -120 0 -140)) hh)) 'users))
           (phone (tag-pict (sbox (freeze (scale-to-height (-bitmap "phone-call.png") (* 0.6 hh)))) 'telephone))
           (phone (cc-superimpose (bhrule (w%->pixels (if (< n 3) 35/100 20/100)) #:thickness 2) phone))
           (hide2 (if (not (= n 1)) values pblank))
           )
      (hc-append tiny-x-sep maker (hide2 phone) (hide2 users))))

(define (penguin-smol)
  (freeze (scale (penguin-row 3) 0.7)))

(define (penguin-logo)
  (scale (designers-and-users 2) 0.72))

(define (endlogo-bg)
  (blank 150 150))

(define (muscle-pict)
  ;; TODO
  (frame (blank 150 150)))

(define (wrench-pict)
  ;; TODO from rational-deep-shallow
  (frame (blank 150 150)))

(define (big-plus-one)
  (cc-superimpose (endlogo-bg) (plus-one)))

(define (other-gt-langs)
  ;; TODO
  (frame (blank 150 150)))

(define (telemetry-design-pict)
  ;; TODO
  (sbox (freeze (scale-to-width (-bitmap "struct.png") (w%->pixels 8/10)))))

(define (future-work-pict)
  (frame (blank 150 150)))

(define (aec-pict)
  (sbox (freeze (scale-to-height (-bitmap "aec.png") 180))))

(define (lshift n pp)
  (hc-append pp (xblank n)))

(define (lindent pp)
  (rshift tiny-x-sep pp))

(define (rshift n pp)
  (hc-append (xblank n) pp))

(define (howtostudy n)
  (define lhs
    (ppict-do
      (vc-append
        pico-y-sep
        (bbox @rmlo{Interviews})
        (bbox @rmlo{Surveys})
        (bbox @rmlo{Experiments}))
      #:go (coord 0 0 'rt #:abs-x (- pico-x-sep)) (scale (titlerm "?") 3)))
  (define mid
    (bbox (hc-append (plus-one) (scale-comment @rmlo{   but Low Bandwidth}))))
  (define rhs
    (bbox
      (vc-append
        pico-y-sep
        (scale-comment @rmlo{this work:})
        @rmem{Telemetry})))
  (hc-append
    medd-x-sep
    (if (< n 1)
      lhs
      (ppict-do (bblur lhs) #:go (coord 1 1 'rt #:abs-y tiny-y-sep) ((if (< n 2) values bblur) mid)))
    ((if (< n 2) pblank values) rhs)))

(define (x-pict size)
  (define outer-color red1-3k1)
  (define inner-color red0-3k1)
  (define line-width% 6)
  ;;
  (define size/2 (/ size 2))
  (define line-width (/ size line-width%))
  (define line-width/2 (/ line-width 2))
  ;;
  (define (draw-x dc% dx dy)
    (define old-brush (send dc% get-brush))
    (define old-pen (send dc% get-pen))
    ;;
    (send dc% set-brush (new brush% [color inner-color]))
    (send dc% set-pen (new pen% [width 1] [color outer-color]))
    ;; draw X from top-left, counterclockwise
    (define path% (new dc-path%))
    (send path% move-to 0 0)
    (send path% line-to (- size/2 line-width/2) size/2)
    (send path% line-to 0 size)
    (send path% line-to line-width size)
    (send path% line-to size/2 (+ size/2 line-width/2))
    (send path% line-to (- size line-width) size)
    (send path% line-to size size)
    (send path% line-to (+ size/2 line-width/2) size/2)
    (send path% line-to size 0)
    (send path% line-to (- size line-width) 0)
    (send path% line-to size/2 (- size/2 line-width/2))
    (send path% line-to line-width 0)
    (send path% close)
    (send dc% draw-path path% dx dy)
    ;;
    (send dc% set-brush old-brush)
    (send dc% set-pen old-pen)
    (void))
  (dc draw-x size size))

(define (the-example-code [n 0])
  (list
      @coderm{local x = { p = 5, q = nil }}
      @coderm{if condition then x.q = 7 end}
      (word-append @coderm{local y = } ((if (< n 2) coderm codeembf) "x.q + x.p"))
      (word-append @coderm{local z = } ((if (< n 1) coderm codeembf) "x.r"))))

(define nocheck-tag (nocheck-codeblock @rmlo{nocheck}))
(define nonstrict-tag (nonstrict-codeblock @rmlo{nonstrict}))
(define strict-tag (strict-codeblock @rmlo{strict}))

(define (code-tag tag pp)
  (ppict-do
    pp
    #:go (coord 1 0 'lt #:abs-x pico-x-sep)
    tag))

(define (low-code-tag pp tag)
  (ppict-do
    pp
    #:go (coord 1 1 'lb #:abs-x pico-x-sep #:abs-y (- 0))
    tag))

(define nonstrict-lang @coderm{--!nonstrict})
(define strict-lang @coderm{--!strict})

(define sww 40)
(define shh 120)

(define (sample-codeblock ff sym)
  (add-hubs (ff (ppict-do (blank sww shh) #:go center-coord (rmlo (symbol->string sym)))) sym))

(define nocheck-sample (sample-codeblock nocheck-codeblock 'NC))
(define nonstrict-sample (sample-codeblock nonstrict-codeblock 'NS))
(define strict-sample (sample-codeblock strict-codeblock 'S))
(define white-sample (cellophane (sample-codeblock white-codeblock '|| ) 0.4))

(define (script-modes-pict)
  (define top
    (ppict-do
      white-sample
      #:go (coord 1/2 7/10 'cc)
      (bbox @rmlo{1 script, 3 typing options})))
  (define bot
    (ht-append smol-x-sep nocheck-sample nonstrict-sample strict-sample))
  (let* ((pp (vc-append medd-y-sep top bot))
         (arr* (list (code-arrow '|-S| lb-find 'NC-N ct-find (* 3/4 turn) (* 3/4 turn) 1/4 1/4 'solid)
                     (code-arrow '|-S| cb-find 'NS-N ct-find (* 3/4 turn) (* 3/4 turn) 1/4 1/4 'solid)
                     (code-arrow '|-S| rb-find 'S-N ct-find (* 3/4 turn) (* 3/4 turn) 1/4 1/4 'solid)
                     )))
    (add-code-arrow* pp arr*)))

(define (codebase-modes-pict)
  (define lbl (bbox (word-append @rmlo{N scripts, } (supers @rmlo{3} @rmlo{N}) @rmlo{  options})))
  (define the-sep tiny-y-sep)
  (ppict-do
    (vc-append
      the-sep
      (apply hc-append the-sep (make-list 4 white-sample))
      (apply hc-append the-sep (make-list 4 white-sample)))
    #:go center-coord lbl))

(define (supers lo hi)
  (vc-append
    (yblank 6)
    (ppict-do
       lo
      #:go (coord 89/100 0 'lc) (scale hi 0.6))))

(define (luau-and-roblox-smol [n 0])
  (if (< n 2)
    (ppict-do
      (luau-logo-smol)
      #:go (coord 1/2 20/100 'cc)
      (roblox-logo-smol))
    (vc-append
      -8
      (scale (roblox-logo-smol) 0.8)
      (luau-logo-smol))))


(define (script-and-codebase [n 0])
    (ht-append
      smol-x-sep
      (script-modes-pict)
      ((if (< n 1) pblank values) (codebase-modes-pict))))

(define (wideimg str)
  (wbox (freeze (scale-to-width (-bitmap str) (w%->pixels 75/100)))))

(define (roblox-studio-pict)
  (freeze (scale-to-square (-bitmap "roblox-studio.png") (w%->pixels 66/100))))

;; -----------------------------------------------------------------------------

(define the-title-str "Privacy-Respecting Type Error Telemetry at Scale")

(define (title-pict) (bbox (rmhi the-title-str)))

(define (sec:title)
  (unless (*export*)
    (pslide
      #:go center-coord
      (freeze (bblur (-bitmap (build-path img-dir "roblox-bg.jpeg"))))))
  (pslide
    #:go center-coord (freeze (bblur (-bitmap (build-path img-dir "roblox-bg.jpeg"))))
    #:go title-coord-m
    (let* ((top (bbox (titlerm the-title-str)))
           (bot (bbox (subtitlerm "<Programming> '24")))
           (scale-amt 0.8)
           (low (vc-append
                  tiny-y-sep
                  (ht-append tiny-y-sep (bbox (scale (brown-logo) scale-amt)) (bbox (scale (cra-logo) scale-amt)))
                  (sbox (roblox-logo))))
           (mid (bbox
                  (author-append
                    @rmem{Ben Greenman}
                    @rmlo{Alan Jeffrey}
                    @rmlo{Shriram Krishnamurthi}
                    @rmlo{Mitesh Shah}
                    )))
           )
      (ppict-do
        top
        #:go (coord 4/100 1 'lt #:abs-y (+ smol-y-sep bigg-y-sep)) (ht-append tiny-y-sep mid low)
        #:go (coord 96/100 1 'rt #:abs-y tiny-y-sep) bot
        ))
    )
  (void))

(define (sec:intro)
  (pslide
    #:go (coord 1/2 1/2 'cb #:abs-y (- pico-y-sep))
    (designers-and-users)
    #:next
    #:go (coord 1/2 1/2 'ct #:abs-y smol-y-sep)
    #:alt ((howtostudy 0))
    #:alt ((howtostudy 1))
    (howtostudy 2)
    )
  (pslide
    #:alt (
      #:go (coord 1/2 1/2 'cb #:abs-y (- pico-y-sep)) (designers-and-users 1)
      #:go (at-find-pict 'maker rt-find 'lc) (thinkbox @rmlo{Deprecate API?})
      #:next
      #:go (at-find-pict 'maker rb-find 'lc #:abs-y 2) (thinkbox @rmlo{Are fatal errors uncommon?})
    )
    #:go (coord 1/2 1/2 'cb #:abs-y (- pico-y-sep)) (designers-and-users 2)
    #:go (at-find-pict 'maker rt-find 'lc) (thinkbox @rmlo{Deprecate API?})
    #:go (at-find-pict 'maker rb-find 'lc #:abs-y 2) (thinkbox @rmlo{Are fatal errors uncommon?})
    #:go (coord 1/2 55/100 'ct)
    ;; TODO nicer right arrow
    (bbox
      (hc-append @rmlo{Telemetry  ==>  Informed Decisions } (plus-one)))
    )
  (pslide
    #:go (coord 1/2 1/2 'cb #:abs-y (- pico-y-sep)) (designers-and-users 2)
    #:go (coord 1/2 55/100 'ct) (bbox @rmlo{But, telemetry can go wrong})
    )
  (pslide
    ;; caution, t gone wrong, transparent telemetry
    #:go (coord 1/2 1/2 'cb #:abs-y (- pico-y-sep))
    (designers-and-users 2)
    #:go (at-find-pict 'telephone cc-find 'cc)
    ;; TODO caution-pict ugly
    (frame (freeze (scale-to-square (-bitmap "caution.png") 180)))
    ;; #:go (at-find-pict 'telephone cb-find 'ct #:abs-y smol-y-sep)
    ;; (bbox @rmrlo{Telemetry is contentious!})
    #:go (at-find-pict 'users lb-find 'ct #:abs-y tiny-y-sep #:sep tiny-y-sep)
    (thinkbox @rmlo{Are you spying on me?})
    #:next
    (yblank tiny-y-sep)
    (hc-append tiny-x-sep
      (thinkbox @rmlo{Personal Info})
      (thinkbox @rmlo{Trade Secrets}))
    )
  (pslide
    #:go (coord 1/2 42/100 'cc)
    (bbox
      (lc-append
        (word-append @rmlo{How to study } @rmem{type errors} @rmlo{ without})
        (word-append @rmlo{revealing any code?})))
    #:go (coord 1/2 75/100 'cc)
    (penguin-logo)
    )
  (pslide
    #:go (coord 1/2 55/100 'cc)
    (bbox
      ;; https://discuss.ocaml.org/t/can-we-improve-type-error-messages/12966
      (code-line-append
        (word-append @coderm{File } @codebf{"main.ml"} @coderm{, line 292, characters 53-70:})
        (word-append @coderm{292 |     } @codebf{textarea [ id "notescontrol"; name "notes"] [txt "%s" c.notes];})
        (word-append @coderm{292 |     } @codebf{                                             ^^^^^^^^^^^^^^^^  })
        (word-append @coderm{Error: This variant expression is expected to have type})
        (word-append @codebf{         ('a, unit, string, node) format4})
        (word-append @coderm{        There is no constructor :: within type } @codebf{format6})))
    #:next
    #:go (coord 1/2 55/100 'cc) (x-pict 340)
    #:go (coord 1/2 22/100 'cc)
      (hc-append smol-x-sep
        (wbox (word-append (x-pict 40) @rmlo{ code}))
        (wbox (word-append (x-pict 40) @rmlo{ types}))
        (wbox (word-append (x-pict 40) @rmlo{ filenames})))
    )
  (pslide
    #:go (coord 1/2 42/100 'cc)
    (bbox
      (lc-append
        (word-append @rmlo{How to study } @rmem{type errors} @rmlo{ without})
        (word-append @rmlo{revealing any code?})))
    #:go (coord 1/2 75/100 'cc)
    (penguin-logo)
    #:go (coord 55/100 20/100 'lt)
    ;; TODO
    (bbox @rmlo{FORMATIVE work, generating hypotheses})
    )
  (void))

(define (sec:rctx)
  (pslide
    ;; roblox, luau, context
    #:go (coord 1/2 18/100 'ct)
    (hc-append
      (vc-append (luau-logo) (yblank smol-y-sep))
      (titlerm (string-append at-sign "  "))
      (roblox-logo))
    #:next
    (bbox @rmlo{2.4 million developers  [Dec'23]})
    (yblank tiny-y-sep)
    (bbox @rmlo{gradual types})
    )
  (pslide
    ;; luau example
    #:go (coord 10/100 6/100 'lt)
    (luau-logo-smol)
    #:go (coord 40/100 20/100 'ct)
    (tag-pict (nocheck-codeblock* (the-example-code)) 'thecode)
    #:next
    #:go (at-find-pict 'thecode rt-find 'lt #:abs-x pico-x-sep #:abs-y (- tiny-y-sep))
    (bbox @rmlo{create a table})
    (yblank tiny-y-sep)
    (bbox @rmlo{update it, maybe})
    (yblank tiny-y-sep)
    (bbox @rmlo{read from table})
    #:next
    #:go (coord 1/2 50/100 'ct)
    (bbox (word-append @codebf{x.r}  @rmlo{ ==> runtime error}))
    (yblank tiny-y-sep)
    (bbox (word-append @codebf{x.q + x.p}  @rmlo{ ==> possible error}))
    )
  (pslide
    ;; nonstrict, strict
    #:go (coord 10/100 6/100 'lt) (luau-logo-smol)
    #:go (coord 40/100 20/100 'ct)
    #:alt (
      (code-tag
        nocheck-tag
        (tag-pict (nocheck-codeblock* (the-example-code)) 'thecode))
      #:go (coord 1/2 50/100 'ct)
      (bbox (word-append @codebf{x.r}  @rmlo{ ==> runtime error}))
      (yblank tiny-y-sep)
      (bbox (word-append @codebf{x.q + x.p}  @rmlo{ ==> possible error}))
    )
    (low-code-tag
      (code-tag
        nonstrict-tag
        (nonstrict-codeblock* (cons nonstrict-lang (the-example-code 1))))
      (bbox @codeembf{Unknown Property}))
    #:next
    (yblank tiny-y-sep)
    (low-code-tag
      (code-tag
        strict-tag
        (strict-codeblock* (cons strict-lang (the-example-code 2))))
      (vl-append
        tiny-y-sep
        (bbox @codeembf{Type Mismatch})
        (bbox @codeembf{Unknown Property})))
    )
  (pslide
    ;;
    #:go (coord 10/100 6/100 'lt) (luau-logo-smol)
    #:go (coord 1/2 30/100 'ct)
    (ptable
      #:ncols 2
      #:row-sep smol-y-sep
      (list
        nocheck-tag (bbox @rmlo{syntax errors})
        nonstrict-tag (bbox @rmlo{high-confidence errors})
        strict-tag (bbox @rmlo{full type analysis})))
    )
  (pslide
    ;; multi-module
    #:go (coord 10/100 6/100 'lt) (luau-logo-smol)
    #:go (coord 1/2 20/100 'ct)
    #:alt ((script-and-codebase 0))
    (script-and-codebase 1)
    )
  (pslide
    ;; roblox studio, type analysis, on the fly, telemetry framework
    #:go (coord 10/100 6/100 'lt) (luau-and-roblox-smol)
    #:go (coord 65/100 09/100 'ct) (roblox-studio-pict)
    ;; #:next
    ;; TODO? highlight the widget
    ;; #:go center-coord
    ;; (rectangle 200 150 #:border-color lite-green #:border-width 4)
    #:next
    #:go (coord 32/100 34/100 'ct)
    ;; TODO stage these, show constraints?
    (lc-append
      (bbox @rmlo{Analysis Widget})
      (bbox (word-append @rmlo{Typechecking } @rmhi{Every} @rmlo{ Keystroke}))
      (bbox @rmlo{Background Analysis})
      (bbox @rmlo{Usage-Data Telemetry}))
    )
  (pslide
    ;; rqs
    ;; TODO awful slide, at least stage RQs
    #:go (coord 1/2 16/100 'ct #:sep tiny-y-sep)
    (bbox @rmlo{Research Topics})
    (yblank pico-y-sep)
    (ll-append
      (bbox
        (ll-append
          (word-append @rmlo{1. Adoption})
          (lindent (scale-comment @rmlo{How many use types?}))
          (lindent (scale-comment (word-append @rmlo{How many } @rmem{mix} @rmlo{ analysis modes?})))
          (lindent (scale-comment (word-append @rmlo{How many } @rmem{change} @rmlo{ analysis modes?})))))
      (bbox
        (ll-append
          (word-append @rmlo{2. Errors and Repairs})
          (lindent (scale-comment @rmlo{Which errors are common?}))
          (lindent (scale-comment (word-append @rmlo{Which errors tend to } @rmem{survive} @rmlo{ edits?})))))
      (bbox
        (ll-append
          (word-append @rmlo{3. Impact on Background Errors})
          (lindent (scale-comment @rmlo{nocheck ==> more background errors?})))))
    )
  (void))

(define (sec:design)
  (pslide
    ;; constraints, design
    #:go heading-coord-m
    (bbox @rmlo{Telemetry Design})
    (yblank smol-y-sep)
    (bbox
      (ptable
        #:ncols 2
        (map scale-comment
          (list
            @rmlo{Who?}  @rmlo{randomly-selected sessions}
            @rmlo{When?} @rmlo{random keystrokes + module switch}
            @rmlo{What?} @rmlo{counts, not code}))))
    #:alt (
      #:go (coord 1/2 55/100 'cc)
      (designers-and-users 2)
     )
    (yblank smol-y-sep)
    (telemetry-design-pict)
    (yblank tiny-y-sep)
    (scale-comment @rmlo{TODO explain step by step})
    ;; TODO explain step-by-step
    ;; give example errors here
    ;; especially the edit range!
    )
  (void))

(define (sec:experiment)
  (pslide
    ;; 3 months 300k sessions
    #:go (coord 1/2 11/100 'ct)
    (wideimg "ft-1.png")
    (yblank tiny-y-sep)
    #:next
    (let ((wnum (lambda (str)
                  (rc-superimpose
                    (xblank (pict-width @rmlo{+340}))
                    (rmlo str))))
          (wtxt (lambda (pp)
                  (lc-superimpose
                    (xblank (pict-width @rmlo{  thousand sessions}))
                    pp))))
      (ll-append
        (bbox (word-append (wnum "3")    (wtxt @rmlo{  months of data})))
        (bbox (word-append (wnum "+1.5") (wtxt @rmlo{  million records})))
        (bbox (word-append (wnum "+340") (wtxt @rmlo{  thousand sessions})))))
    )
  (pslide
    ;; rq1 adoption
    #:go (coord 1/2 11/100 'ct)
    (bbox @rmlo{1. Adoption})
    (yblank tiny-y-sep)
    ;; TODO pics, staging
    (wideimg "ft-5.png")
    )
  (pslide
    #:go (coord 1/2 11/100 'ct)
    (bbox @rmlo{2. Errors})
    (yblank tiny-y-sep)
    ;; TODO smaller, focus!
    (wideimg "ft-7.png")
    )
  (pslide
    #:go center-coord
    (bbox @rmlo{too complex})
    )
  (pslide
    #:go (coord 1/2 11/100 'ct)
    (bbox @rmlo{2. Errors + Repairs})
    (yblank tiny-y-sep)
    ;; TODO something
    ;; TODO colors for NC NS S
    (wideimg "ft-9.png")
    )
  (pslide
    #:go (coord 1/2 11/100 'ct)
    (bbox @rmlo{3. Types vs. Background Errors})
    (yblank tiny-y-sep)
    ;; TODO something
    ;; TODO colors for NC NS S
    (wideimg "ft-5.png")
    (yblank tiny-y-sep)
    (bbox @rmlo{same % as adoption rates, yikes})
    )
  (pslide
    #:go (coord 1/2 11/100 'ct)
    (bbox @rmlo{3. Types vs. Background Errors})
    (yblank tiny-y-sep)
    ;; TODO something
    (wideimg "ft-10.png")
    )
  (pslide
    #:go (coord 1/2 11/100 'ct)
    (bbox @rmlo{3. Types vs. Background Errors})
    (yblank tiny-y-sep)
    (bbox
      (ll-append
        @rmlo{Uh oh ...}
        (word-append @rmlo{3% of strict increase } @rmem{module} @rmlo{ type errors but not background errors})
        (word-append @rmlo{16% of strict increase } @rmem{global} @rmlo{ type errors but not background})
        ))
    (yblank tiny-y-sep)
    (bbox @rmlo{strict is too strict for data model})
    )
  (pslide
    #:go (coord 65/100 09/100 'ct) (roblox-studio-pict)
    #:go center-coord
    (bbox @rmlo{highlight DATA widget, use icons for terrain, music, character})
    )
  (pslide
    ;; findings
    #:go (coord 1/2 16/100 'ct #:sep tiny-y-sep)
    (bbox @rmlo{Findings})
    (yblank pico-y-sep)
    ;; TODO stage ... exictiement??!
    ;; TODO at least -1 for adoption, +1 errors, =1 bg
    (ll-append
      (bbox
        (ll-append
          (word-append @rmlo{1. Adoption})
          (lindent (scale-comment @rmlo{10% use types, a mere 1% use strict}))
          (lindent (scale-comment @rmlo{<0.15% mix analysis modes}))
          (lindent (scale-comment @rmlo{<0.13% change modes}))
          ;; roughly even b/w down (0.07) and upgrade (0.05)
          ))
      (bbox
        (ll-append
          (word-append @rmlo{2. Errors and Repairs})
          (lindent (scale-comment @rmlo{common errors: syntax (50%), arity (2%), option unpacking (2%)}))
          (lindent (hc-append tiny-x-sep (scale-comment @rmlo{internal limits are rare}) (plus-one)))
          (lindent (scale-comment @rmlo{errors rarely pile up}))
          ))
      (bbox
        (ll-append
          (word-append @rmlo{3. Impact on Background Errors})
          (lindent (scale-comment @rmlo{no correlation})))))
    )
  (void))

(define (sec:takeaways)
  (pslide
    ;; threats
    ;; - sampling incomplete
    ;; - edit ranges coarse
    ;; - stx dominate global view
    ;; - many errors reported, no idea which if any users targeted
    )
  (pslide
    ;; roblox takeaways
    #:go (coord 10/100 06/100 'lt) (luau-and-roblox-smol 2)
    #:go (coord 26/100 25/100 'lt)
    (bbox (hc-append tiny-x-sep (muscle-pict) @rmlo{Make nonstrict the default!}))
    (yblank smol-y-sep)
    (bbox
        (hc-append
          tiny-x-sep
          (wrench-pict)
          (ll-append
            @rmlo{Strict needs work}
            (lindent (comment-scale @rmlo{low adoption ==> inexpressive?}))
            (lindent (comment-scale @rmlo{data model types})))))
    )
  (pslide
    ;; general
    #:go (coord 90/100 06/100 'rt) (penguin-smol)
    #:go (coord 17/100 25/100 'lt)
    (bbox
      (hc-append tiny-x-sep (big-plus-one)
        (ll-append
          @rmlo{Lite telemetry ==> useful analyses}
          (lindent (comment-scale @rmlo{gradual adoption}))
          (lindent (comment-scale @rmlo{error frequency}))
          (lindent (comment-scale @rmlo{repairs})))))
    (yblank smol-y-sep)
    #:next
    (bbox
      (hc-append
        tiny-x-sep
        (other-gt-langs)
        @rmlo{Lots to learn about types in practice}))
    )
  (pslide
    ;; public data
    #:go (coord 1/2 14/100 'ct)
    (hc-append
      tiny-x-sep
      (aec-pict)
      (lc-append
        (bbox @coderm{https://zenodo.org/doi/10.5281/zenodo.10275213})
        (bbox @rmlo{Data + Analysis Scripts})))
    (yblank medd-y-sep)
    (bbox
      (hc-append
        tiny-x-sep
        (future-work-pict)
        @rmlo{Statistical models of programmers}))
    )
  (pslide
    #:go center-coord (freeze (bblur (-bitmap (build-path img-dir "roblox-bg.jpeg"))))
    #:go center-coord (bbox @rmlo{The End})
    )
  (pslide
    ;; final summary
    #:go (coord 10/100 06/100 'lt) (luau-and-roblox-smol 2)
    #:go (coord 90/100 06/100 'rt) (penguin-smol)
    #:go (coord 1/2 48/100 'cc) (telemetry-design-pict)
    ;; ??? data pict?
    )
  (void))

(define (sec:extra)
  ;; big tables???
  ;; ft-1 ... ft-11.png
  (void))

;; -----------------------------------------------------------------------------

(define (do-show)
  [set-spotlight-style! #:size 60 #:color (color%-update-alpha highlight-brush-color 0.6)]
  [set-page-numbers-visible! (if #true #true #false)]
  [current-page-number-font page-font]
  [current-page-number-color white]
  ;; --
  (parameterize ((*export* (and (member "-x" (vector->list (current-command-line-arguments))) #true))
                 (current-slide-assembler bg-bg))
    (sec:title)
    (sec:intro)
    (sec:rctx)
    (sec:design)
    (sec:experiment)
    (sec:takeaways)
    (sec:extra)
    (when (*export*) (pslide))
    (void))
  (void))

(module+ main
  (do-show))

;; =============================================================================

(module+ raco-pict (provide raco-pict)
         ;; (define client-w 984) (define client-h 728) ;; 4:3
         (define client-w 1320) (define client-h 726) ;; 16:9 sort of, too thin
         (define raco-pict
  (ppict-do
    (make-bg client-w client-h)

    #:go (coord 1/2 16/100 'ct #:sep tiny-y-sep)
    (bbox @rmlo{Threats})


    ;; bare minimum example errors
; TypeMismatch 	 Basic type error.
; SyntaxError 	 Basic parse error, e.g., \code{for if end}.
; UnknownProperty 	 Referenced an invalid field or method.
; OnlyTablesCanHaveMethods 	 Tried to attach a method to a non-table.
; CannotExtendTable 	 Tried to extend a sealed table.
; TypesAreUnrelated 	 Failed cast, unify, or subtype.
; CountMismatch 	 Arity mismatch for a function.
; IncorrectGenericParamCount 	 Arity mismatch for a generic type.
; CodeTooComplex 	 Type analysis failed~(\cref{s:code-too-complex}).
; GenericError 	 Generic label for other non-type errors, e.g., looping over an unordered table.




  )))
