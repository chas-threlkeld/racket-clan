#lang racket

(require (only-in ffi/vector
                  s16vector-ref)
         (only-in rsound
                  rsound-data
                  rsound-start))

(provide wav
         format-sample
         right-getter
         left-getter
         abs-max-from
         rasterize-column
         cha-to-timestamp)

(define (cha-to-timestamp cha)
  (let* ([timestamp (string->number cha)]
         [millis (~r (modulo timestamp 1000) #:min-width 3 #:pad-string "0")]
         [seconds (~r (modulo (floor (/ timestamp 1000)) 60) #:min-width 2 #:pad-string "0")]
         [minutes (~r (modulo (floor (/ timestamp 60000)) 60) #:min-width 2 #:pad-string "0")]
         [hours (~r (floor (/ timestamp 3600000)))])
    (string-append hours ":" minutes ":" seconds "." millis)))

(define wav "2018-01-29-session-1-loud.wav")
(define s16max #x7fff)
(define channels 2)

(define (abs-max-from getter sound limit)
  (for/fold ([abs-max 0])
            ([i (in-range limit)])
    (max (abs (getter sound i)) abs-max)))

(define (frame->sample f left?)
  (+ (* f channels) (if left? 0 1)))

(define (rs-extractor sound frame left? scale-fun)
  (scale-fun (s16vector-ref (rsound-data sound)
                            (frame->sample (+ (rsound-start sound) frame)
                                           left?))))

(define (rs-ith/left/s16 sound frame)
  (rs-extractor sound frame #t (lambda (x) x)))

(define (rs-ith/right/s16 sound frame)
  (rs-extractor sound frame #t (lambda (x) x)))

(define (interpolate get-sample n)
  (let* ([f1 (floor n)]
         [frac (- n f1)])
    (+ (* (- 1 frac) (get-sample f1)) (* frac (get-sample (+ f1 1))))))

(define (rasterize-column getter left-edge right-edge)
  (let* ([left-edge-left-value (interpolate getter left-edge)]
         [left-edge-right-value (interpolate getter right-edge)]
         [in-between-left-values (for/list ([i (in-range
                                                (ceiling left-edge)
                                                (+ 1 (floor right-edge)))])
                                   (getter i))]
         [all-vals (cons left-edge-left-value
                         (cons left-edge-right-value in-between-left-values))]
         [left-min (apply min all-vals)]
         [left-max (apply max all-vals)])
    (values left-min left-max)))

(define (left-getter sound i) (/ (rs-ith/left/s16 sound i) s16max))
(define (right-getter sound i) (/ (rs-ith/right/s16 sound i) s16max))

(define digits-to-print 4)

(define (format-sample n)
  (real->decimal-string n digits-to-print))
