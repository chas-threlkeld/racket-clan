#lang racket/gui

(require "util.rkt"
         (only-in "io.rkt"
                  file-text)
         (only-in rsound
                  play
                  rs-frames
                  rs-read/clip)
         racket/draw
         framework
         racket/class
         racket/match)

(define (get-stamps)
  (define line-start (send text get-start-of-line (send text get-end-position)))
  (define line-end (send text find-newline 'forward line-start))
  (if (send text find-string "•" 'forward line-start line-end)
      (let* ([time-stamp-start (send text find-string "•" 'forward line-start line-end)]
             [time-stamp-mid (send text find-string "_" 'forward time-stamp-start line-end)]
             [start-stamp (substring
                           (send text get-text time-stamp-start time-stamp-mid)
                           1)]
             [end-stamp (substring
                         (send text get-text time-stamp-mid line-end)
                         1 (- (- line-end time-stamp-mid) 2))])
        (if (string=? start-stamp end-stamp)
            (let* ([start-num (string->number start-stamp)]
                   [end-num (+ start-num 5000)]
                   [end (~a end-num)])
              (values start-stamp end))
            (values start-stamp end-stamp)))
      (values "0" "100")))

;; SOUND-CANVAS%
;;
;; the canvas that draws a sound

(define sound-canvas%
  (class canvas%
    (init-field len)
    (init-field frame-num-text)
    (init-field y-value-text)
    (init-field frames-per-pixel)

    (define/public (get-frames-per-pixel)
      frames-per-pixel)

    ;; given a number of frames per pixel, compute the required
    ;; virtual canvas width
    (define (fpp->virtual-width frames-per-pixel)
      (ceiling (/ len frames-per-pixel)))

    (unless (positive-integer? len)
      (raise-argument-error 'sound-canvas-init
                            "positive integer" 0 len))

    (unless (< 0 frames-per-pixel)
      (raise-argument-error 'sound-canvas-init
                            "positive number" 0 frames-per-pixel))

    (unless (dimension-integer?
             (fpp->virtual-width frames-per-pixel))
      (raise-argument-error 'sound-canvas-init
                            "number implying legal canvas width"
                            0 frames-per-pixel))

    (define cur-mouse-x 0)

    (inherit get-width get-height get-parent init-auto-scrollbars
             get-view-start get-client-size get-virtual-size)

    (define (get-client-width)
      (define-values (w _) (get-client-size))
      w)
    
    (define/public (update-drawing)
      (define start (string->number (send (send start-text get-editor) get-text)))
      (define end (string->number (send (send end-text get-editor) get-text)))
      (define sound (rs-read/clip wav (* 16 start) (* 16 end)))
      (define len (rs-frames sound))
      (define dc (send this get-dc))
      (define display-max (max (abs-max-from left-getter sound len) (abs-max-from right-getter sound len)))
  
      (define-values (view-start-x _1) (send this get-view-start))
      (define-values (client-width _2) (send this get-client-size))
      (define-values (virtual-canvas-width _3) (send this get-virtual-size))
      (define fpp (/ len virtual-canvas-width))
      (define data-left (floor (* fpp view-start-x)))
      (define frames (floor (* fpp client-width)))
      (define proposed-data-right (+ data-left frames))
      (define actual-data-right (min len proposed-data-right))
      (define stop-pixel (- (/ actual-data-right fpp) view-start-x))
      (let* ([h (- (send this get-height) 1)]
             [half-h (floor (/ h 2))]
             [h-scale (/ (- frames 1) (- client-width 1))]
             [v-scale (/ (/ half-h 2) display-max)]
             [upper-centerline (* 1/2 half-h)]
             [lower-centerline (* 3/2 half-h)]
             [offset-left-getter (lambda (i) (left-getter sound (+ i data-left)))]
             [offset-right-getter (lambda (i) (right-getter sound (+ i data-left)))])
        (for ([i (in-range 1 (- stop-pixel 1))])
          (let ([raster-left (* h-scale (- i 1/2))]
                [raster-right (* h-scale (+ i 1/2))])
            (let*-values ([(left-min left-max)
                           (rasterize-column offset-left-getter
                                             raster-left raster-right)]
                          [(right-min right-max)
                           (rasterize-column offset-right-getter
                                             raster-left raster-right)])
              (define (num->pixel/left n)
                (inexact->exact (floor (- upper-centerline (* v-scale n)))))
              (define (num->pixel/right n)
                (inexact->exact (floor (- lower-centerline (* v-scale n)))))
              (send dc draw-line
                (+ view-start-x i) (num->pixel/left left-max)
                (+ view-start-x i) (num->pixel/left left-min))
              (send dc draw-line
                    (+ view-start-x i) (num->pixel/right right-max)
                    (+ view-start-x i) (num->pixel/right right-min)))))
        #f))
    
    (define/override (on-char evt)
      (define key-code (send evt get-key-code))
      
      (define client-width (get-client-width))
      (define-values (view-start-x _1) (get-view-start))
      (define data-left (floor (* frames-per-pixel client-width)))
      (define frames (floor (* frames-per-pixel client-width)))
      ;; given an x coordinate, return the corresponding frame
      (define (pixel->frame x)
        (+ data-left (floor (* frames-per-pixel x))))

      (match key-code
        [#\+ (set-frames-per-pixel! (/ frames-per-pixel 2))]
        [#\- (set-frames-per-pixel! (* frames-per-pixel 2))]
        ['f5 (begin
               (send (send sound-canvas get-dc) erase)
               (update-drawing))]
        [other #f]))
    
    (define (set-frames-per-pixel! fpp)
      (unless (< 0 fpp)
        (raise-argument-error 'set-frames-per-pixel!
                              "positive number" 0 fpp))
      (define virtual-width (fpp->virtual-width fpp))
      (cond [(dimension-integer? virtual-width)
             (set! frames-per-pixel fpp)
             (init-auto-scrollbars virtual-width #f 0.0 0.0)]
            [else
             (message-box
              "Too Much Zoom!"
              (string-append
               "Zooming in to this level requires a virtual "
               "canvas size that the platform can't handle. "
               "To zoom in further, cut the source data into "
               "smaller pieces (e.g., using (clip ...))."))]))

    (define/override (on-event evt)
      (define evt-type (send evt get-event-type))
      (set! cur-mouse-x (send evt get-x))
      (define client-width (get-client-width))
      (define-values (view-start-x _1) (get-view-start))
      (define data-left (floor (* frames-per-pixel view-start-x)))
      ;; given an x-coordinate, return the corresponding frame
      (define (pixel->frame x)
        (+ data-left (floor (* frames-per-pixel x))))

      (define start (string->number (send (send start-text get-editor) get-text)))
      (define end (string->number (send (send end-text get-editor) get-text)))
      (define sound (rs-read/clip wav (* 16 start) (* 16 end)))
      
      (define x (min (max 0 (send evt get-x)) (- (get-client-width) 1)))
      (define scaled-x (pixel->frame x))
      (define y (send evt get-y))
      (define y-val
        (cond [(< scaled-x len)
               (format-sample
                (if (> y (/ (get-height) 2))
                    (right-getter sound scaled-x)
                    (left-getter sound scaled-x)))]
              [else "undefined"]))
      (define frame-num-str
        (cond [(< scaled-x len) scaled-x]
              [else "undefined"]))
      (match evt-type
        ['left-down (send s-message set-label (format "Start: ~a" cur-mouse-x))]
        ['left-up (send e-message set-label (format "End: ~a" cur-mouse-x))]
        [else #f])
      ;; (send s-message set-label
      ;;       (format "frame #: ~a" frame-num-str))
      ;; (send s-message begin-edit-sequence #f)
      ;; (send s-message erase)
      ;; (send s-message insert
      ;;       (format "frame #: ~a" frame-num-str))
      ;; (send s-message end-edit-sequence)
      (send y-value-text begin-edit-sequence #f)
      (send y-value-text erase)
      (send y-value-text insert
            (format "y value: ~a" y-val))
      (send y-value-text end-edit-sequence))

    (super-new)
    (init-auto-scrollbars (fpp->virtual-width frames-per-pixel)
                          #f 0.0 0.0)         
    )
  )

(define dialog (new frame%
                    [label "CLAN"]
                    [width 800]))


(define editor (new editor-canvas% [parent dialog]
                    [label "editor"]
                    [min-height 600]))

(define clan-font (make-object font% 12 "CAfont" 'default))

(define clan-text% (text:line-numbers-mixin
                    (text:basic-mixin
                     (editor:standard-style-list-mixin
                      (editor:basic-mixin text%)))))

(define text (new clan-text%))

(send (send editor get-dc) set-font clan-font)

(send editor set-editor text)

(define menu-bar (new menu-bar% [parent dialog]))
(define m-file (new menu% [label "File"] [parent menu-bar]))
(define m-edit (new menu% [label "Edit"] [parent menu-bar]))
(define m-font (new menu% [label "Font"] [parent menu-bar]))
(define m-insert (new menu% [label "Insert"] [parent menu-bar]))

(new menu-item%
     [label "Open"]
     [parent m-file]
     [callback (lambda (canvas button)
                 (define file-name (finder:get-file))
                 (if file-name
                     (send text insert (call-with-input-file file-name
                                         port->string #:mode 'text))
                     #f))])

(new menu-item%
     [label "Save As"]
     [parent m-file]
     [callback (lambda (canvas button)
                 (define file-name (finder:put-file))
                 (call-with-output-file file-name
                   (lambda (out)
                     (write (send text get-text) out))
                   #:exists 'replace))])

(new menu-item%
     [label "↑ Shift to high pitch"]
     [parent m-insert]
     [callback (lambda (canvas button)
                 (send text insert "↑" (send text get-end-position)))])

(append-editor-operation-menu-items m-edit #f)
(append-editor-font-menu-items m-font)
(send text set-max-undo-history 100)

(define h-pane (new horizontal-pane% [parent dialog]))

(define h-left (new horizontal-pane% [parent h-pane]))

(define start-text (new text-field% [parent h-left]
                        [label "Start"]
                        [init-value "1000"]
                        [min-width 200]
                        [stretchable-width #f]))


(define h-center (new horizontal-pane% [parent h-pane]
                      [alignment (list 'center 'center)]))

(define s-message (new message% [parent h-center]
                       [label "xxx"]))


(define e-message (new message% [parent h-center]
                       [label "yyy"]))

(define h-right (new horizontal-pane% [parent h-pane]
                     [alignment (list 'left 'center)]))

(define end-text (new text-field% [parent h-right]
                      [label "End"]
                      [init-value "5000"]
                      [min-width 200]
                      [stretchable-width #f]))

(define sound-canvas
  (let* ([start (string->number (send (send start-text get-editor) get-text))]
         [end (string->number (send (send end-text get-editor) get-text))]
         [sound (rs-read/clip wav start end)]
         [len-sound (rs-frames sound)])
    (new sound-canvas% [parent dialog]
         [label "Visualization"]
         [horiz-margin 4]
         [min-height 200]
         [len len-sound]
         [frame-num-text (new text%)]
         [y-value-text (new text%)]
         [style '(hscroll)]
         [frames-per-pixel (/ len-sound 800)]
         [stretchable-height #f])))

(define m (new message% [parent dialog]
               [label "hello"]))

(define (play-turn)
  ;;(define-values (start-stamp end-stamp) (get-stamps))
  (let* ([line-start (send text get-start-of-line (send text get-end-position))]
         [line-end (send text find-newline 'forward line-start)]
         [time-stamp-start (send text find-string "•" 'forward line-start line-end)]
         [time-stamp-mid (send text find-string "_" 'forward time-stamp-start line-end)]
         [start-stamp (substring
                       (send text get-text time-stamp-start time-stamp-mid)
                       1)]
         [end-stamp (substring
                     (send text get-text time-stamp-mid line-end)
                     1 (- (- line-end time-stamp-mid) 2))]
         [sound (rs-read/clip wav
                              (* 16 (string->number start-stamp))
                              (* 16 (string->number end-stamp)))])
    (send start-text set-value start-stamp)
    (send end-text set-value end-stamp)
    (send m set-label (string-append (cha-to-timestamp start-stamp) " - " (cha-to-timestamp end-stamp)))
    (send (send sound-canvas get-dc) erase)
    (send sound-canvas update-drawing)
    (play sound)))

(let ([line-start (send text get-start-of-line (send text get-end-position))])
  (send m set-label (~a (send text find-newline 'forward line-start))))


(define redraw (new button% [parent dialog]
                    [label "Redraw"]
                    [callback (lambda (button event)
                                  (play-turn))]))

(send text insert (call-with-input-file "2018-01-29-session-1.cha" port->string #:mode 'text))

(send dialog show #t)

