#lang racket

(require pict)

(provide plot-images)

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




