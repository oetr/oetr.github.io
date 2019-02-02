#lang racket
(require racket/draw)
(require racket/math)
(require math/matrix)
(require pict)

(provide clear
         forward
         back
         turn

         ;; pen
         set-pen
         penup
         pendown

         ;; draw, show
         draw-turtle
         show
         plot-images
         
         )

(define *canvas* (make-bitmap 300 300))
(define *dc* (new bitmap-dc% [bitmap *canvas*]))
(define *pen* #f)
(define *pendown?* #t)

(define *min-x* +inf.0)
(define *max-x* -inf.0)
(define *min-y* +inf.0)
(define *max-y* -inf.0)

(define (assemble-rt-mat dx dy theta)
  (define theta-rads (degrees->radians theta))
  (matrix [[(cos theta-rads) (- (sin theta-rads)) dx]
           [(sin theta-rads) (cos theta-rads)     dy]
           [0.0             0.0                   1.0]]))

(define *rt-mat* (assemble-rt-mat 0.0 0.0 180))

(define (clear (width 400) (height 400))
  (set! *canvas* (make-bitmap width height))
  (set! *dc* (new bitmap-dc% [bitmap *canvas*]))
  (send *dc* set-pen (make-color 0 0 0) 1 'solid)
  (send *dc* set-brush (make-color 0 0 0) 'solid)
  ;;(send *dc* set-smoothing 'aligned)
  (send *dc* set-smoothing 'smoothed)
  (define *pendown?* #t)
  (set! *rt-mat* (assemble-rt-mat (/ width 2.0) (/ height 2.0) 180))
  (set! *min-x* +inf.0)
  (set! *max-x* -inf.0)
  (set! *min-y* +inf.0)
  (set! *max-y* -inf.0))

(define BASE-TURTLE-POINTS
  (matrix [[0.0   0.0 1.0] ;; center point
           [0.0   8.0 1.0] ;; upper point
           [6.0  -8.0 1.0] ;; lower right point
           [-6.0 -8.0 1.0]]))

(define (update-rt-mat! dx dy theta)
  (set! *rt-mat*
        (matrix* *rt-mat* (assemble-rt-mat dx dy theta))))

(define (update-rt-mat dx dy theta)
  (matrix* *rt-mat* (assemble-rt-mat dx dy theta)))

(define (update-min-max! . args)
  (for ([pnt args])
    (define x (car pnt))
    (define y (cdr pnt))
    (set! *min-x* (min *min-x* x))
    (set! *max-x* (max *max-x* x))
    (set! *min-y* (min *min-y* y))
    (set! *max-y* (max *max-y* y))))

(define (move+draw n)
  (define turtle-points-before
    (matrix-transpose (matrix* *rt-mat* (matrix-transpose BASE-TURTLE-POINTS))))
  (define x0 (matrix-ref turtle-points-before 0 0))
  (define y0 (matrix-ref turtle-points-before 0 1))
  (update-rt-mat! 0 n 0)
  (define turtle-points-after
    (matrix-transpose (matrix* *rt-mat* (matrix-transpose BASE-TURTLE-POINTS))))
  (define x1 (matrix-ref turtle-points-after 0 0))
  (define y1 (matrix-ref turtle-points-after 0 1))
  (send *dc* draw-line x0 y0 x1 y1)
  ;; save the minimum bounding box
  (update-min-max! (cons x0 y0) (cons x1 y1)))

(define (forward n) (move+draw n))
(define (back n) (move+draw (- n)))

(define (turn degrees)
  (update-rt-mat! 0.0 0.0 degrees))

(define (draw-turtle dc offset-x offset-y)
  (define turtle-points
    (matrix-transpose
     (matrix* (matrix* (assemble-rt-mat offset-x offset-y 0.0)
                       *rt-mat*)
              (matrix-transpose BASE-TURTLE-POINTS))))
  (define point-x (matrix-ref turtle-points 0 0))
  (define point-y (matrix-ref turtle-points 0 1))
  ;; draw center point
  (send dc draw-point point-x point-y)
  ;; draw lines around
  (send dc draw-line
        (matrix-ref turtle-points 1 0)
        (matrix-ref turtle-points 1 1)
        (matrix-ref turtle-points 2 0)
        (matrix-ref turtle-points 2 1))
  (send dc draw-line
        (matrix-ref turtle-points 2 0)
        (matrix-ref turtle-points 2 1)
        (matrix-ref turtle-points 3 0)
        (matrix-ref turtle-points 3 1))
  (send dc draw-line
        (matrix-ref turtle-points 1 0)
        (matrix-ref turtle-points 1 1)
        (matrix-ref turtle-points 3 0)
        (matrix-ref turtle-points 3 1)))

(define (set-pen r g b (a 1.0))
  (send *dc* set-pen (make-color r g b) 1 'solid))

(define (penup)
  (set! *pendown?* #f)
  (set! *pen* (send *dc* get-pen))
  (send *dc* set-pen (make-color 0 0 0) 0 'transparent))

(define (pendown)
  (unless *pendown?*
    (set! *pendown?* #t)
    (send *dc* set-pen *pen*)))

(define (show)
  (define w (send *canvas* get-width))
  (define h (send *canvas* get-height))
  (define margin 10)
  (define x0 (max 0 (- (inexact->exact (round *min-x*)) margin)))
  (define y0 (max 0 (- (inexact->exact (round *min-y*)) margin)))
  (define x1 (min w (+ (inexact->exact (round *max-x*)) margin)))
  (define y1 (min h (+ (inexact->exact (round *max-y*)) margin)))
  (define region-w (- x1 x0))
  (define region-h (- y1 y0))
  (pretty-print  *rt-mat*)
  (printf "(~a, ~a) - (~a, ~a)\n" x0 y0 x1 y1)
  
  
  (define pixels (make-bytes (* region-w region-h 4)))
  (send *canvas* get-argb-pixels x0 y0 region-w region-h pixels)
  (define bm (make-bitmap region-w region-h))
  (send bm set-argb-pixels 0 0 region-w region-h pixels)
  (define dc (new bitmap-dc% [bitmap bm]))
  (draw-turtle dc (- x0) (- y0))
  bm)

(module+ test
  (clear 1000 1000)
  (forward 100)
  (turn 90)
  (forward 150)
  (turn -45)
  (back 100)
  (turn -45)
  (penup)
  (forward 100)
  (pendown)
  (forward 100)
  (show))


;; plot when cols are accumulated
(define (plot-images cols lof-picts)
  (define accumulated-pict #f)
  (define accumulated-row  #f)
  (for ([pict lof-picts]
        [i (length lof-picts)])
    (if accumulated-row
        (set! accumulated-row (hc-append accumulated-row pict))
        (set! accumulated-row pict))
    (when (zero? (modulo (+ i 1) cols))
      (if accumulated-pict
          (set! accumulated-pict (vc-append accumulated-pict
                                            accumulated-row))
          (set! accumulated-pict accumulated-row))
      (set! accumulated-row #f)))
  ;; plot the rest of accumulated row
  (if accumulated-row
      (vl-append accumulated-pict accumulated-row)
      accumulated-pict))

(module+ test
  (plot-images 2
               (list
                (colorize (filled-rectangle 60 30) "tomato")
                (colorize (disk 45) "red")
                (colorize (filled-rectangle 60 30) "tomato")
                (colorize (disk 45) "cornflower blue")
                (colorize (disk 45) "cornflower blue"))))

