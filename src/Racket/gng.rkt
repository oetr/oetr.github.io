#lang racket
(require plot
         math/flonum)

(require "./plot-utilities.rkt")

(struct unit ([position #:mutable] [error #:mutable] [edges #:mutable] id))

(define (make-get-next-id)
  (define unit-next-unique-id 0)
  (λ ()
    (set! unit-next-unique-id (+ unit-next-unique-id 1))
    (- unit-next-unique-id 1)))

(define generate-id! (make-get-next-id))

(define (make-unit position error edges)
  (unit position error edges (generate-id!)))

(struct edge (unit1 unit2 age) #:mutable)

;; find the two nearest units to the data
(define (find-two-nearest data units)
  (define first-min 0)
  (define second-min 0)
  (define first-unit null)
  (define second-unit null)
  (for ([a-unit units])
    (define new-distance (euclidean-distance (unit-position a-unit) data))
    (cond [(null? first-unit)
           (set! first-min new-distance)
           (set! first-unit a-unit)]
          [(< new-distance first-min)
           (set! second-min first-min)
           (set! second-unit first-unit)
           (set! first-min new-distance)
           (set! first-unit a-unit)]
          [(or (null? second-unit) (< new-distance second-min))
           (set! second-min new-distance)
           (set! second-unit a-unit)]))
  (list first-unit second-unit))

;; finds the edge connecting two units
(define (find-edge u1 u2)
  (define edges (unit-edges u1))
  (define id1 (unit-id u1))
  (define id2 (unit-id u2))
  (let loop ([edges edges])
    (if (empty? edges)
        empty
        (let ([edge (car edges)])
          (let ([n1 (edge-unit1 edge)]
                [n2 (edge-unit2 edge)])
            (if (or (and (= id1 (unit-id n1))
                         (= id2 (unit-id n2)))
                    (and (= id1 (unit-id n2))
                         (= id2 (unit-id n1))))
                edge
                (loop (cdr edges))))))))

(define (euclidean-distance p0 p1)
  (sqrt (flvector-sum (flvector-sqr (flvector- p0 p1)))))

;; 4. increment the age of an edge
(define (increment-age! edge)
  (set-edge-age! edge (+ (edge-age edge) 1)))

;; 5. Update local error
(define (update-local-error! unit error)
  (set-unit-error! unit (+ (unit-error unit) error)))

;; 6. Find neighbors
;; search the edges and return the the units that are not the provided unit
(define (find-neighbors a-unit edges)
  (for/list ([edge edges])
    (define unit1 (edge-unit1 edge))
    (if (= (unit-id unit1) (unit-id a-unit))
        (edge-unit2 edge)
        unit1)))

;; units without edges have edge count of zero
(define (leave-units-with-edges units)
  (for/list ([a-unit units]
             #:when (not (empty? (unit-edges a-unit))))
    a-unit))

;; 6. Move a unit towards the input by a fraction
(define (move-by-a-fraction! a-unit x epsilon)
  (define position (unit-position a-unit))
  (set-unit-position! a-unit
                      (flvector+
                       position
                       (flvector-scale
                        (flvector- x
                                   (unit-position a-unit))
                        epsilon))))

;; 4. set the age to zero
(define (reset-age! edge) (set-edge-age! edge 0))

;; 8. remove edges whose age is larger than a-max
;; returns units whose edges were removed
(define (remove-edge! edge)
  (define unit1 (edge-unit1 edge))
  (define unit2 (edge-unit2 edge))
  (set-unit-edges! unit1 (remove edge (unit-edges unit1)))
  (set-unit-edges! unit2 (remove edge (unit-edges unit2))))

(define (remove-edges! edges)
  (for ([edge edges]) (remove-edge! edge)))

(define (insert-unit-between! u1 u2)
  (define new-unit
    (make-unit (flvector-scale
                (flvector+ (unit-position u1)
                           (unit-position u2))
                0.5)
               (max (unit-error u1)
                    (unit-error u2))
               empty))
  (define new-edge1 (edge new-unit u1 0.0))
  (define new-edge2 (edge new-unit u2 0.0))
  (set-unit-edges! new-unit (list new-edge1 new-edge2))
  (set-unit-edges! u1 (cons new-edge1 (unit-edges u1)))
  (set-unit-edges! u2 (cons new-edge2 (unit-edges u2)))
  (remove-common-edge! u1 u2)
  new-unit)

(define (remove-common-edge! u1 u2)
  (define id1 (unit-id u1))
  (define id2 (unit-id u2))
  (define (remove-edges edges)
    (define new-edges empty)
    (for ([edge edges])
      (define unit-id1 (unit-id (edge-unit1 edge)))
      (define unit-id2 (unit-id (edge-unit2 edge)))
      (unless (or (and (= id1 unit-id1)
                       (= id2 unit-id2))
                  (and (= id1 unit-id2)
                       (= id2 unit-id1)))
        (set! new-edges (cons edge new-edges))))
    new-edges)
  (set-unit-edges! u1 (remove-edges (unit-edges u1)))
  (set-unit-edges! u2 (remove-edges (unit-edges u2))))


(define (make-initial-network dims)
  (define edges empty)
  (define initial-age 0)
  (define units
    (build-list 2
                (lambda (n)
                  (make-unit
                   (build-flvector dims (lambda _ (random)))
                   0.0 edges))))
  (define initial-edge (edge (first units) (second units) initial-age))
  (set-unit-edges! (first units) (list initial-edge))
  (set-unit-edges! (second units) (list initial-edge))
  units)

;; 9. Insert a new unit
(define (insert-new-unit! units alpha)
  ;; 9. Find the unit with maximum accumulated error and its edges
  (define highest-error-unit (argmax unit-error units))
  (define emanating-edges (unit-edges highest-error-unit))
  (define highest-error-neighbor
    (argmax unit-error (find-neighbors highest-error-unit emanating-edges)))
  ;; 9. Decrease the error of unit and its neighbors
  (set-unit-error! highest-error-unit
                   (* alpha (unit-error highest-error-unit)))
  (set-unit-error! highest-error-neighbor
                   (* alpha (unit-error highest-error-neighbor)))
  ;; 9. Insert a new unit between highest and its neighbor
  (define new-unit (insert-unit-between! highest-error-unit
                                         highest-error-neighbor))
  (set! units (cons new-unit units))
  units)

;; final algorithm
(define (gng-update! units data epsilon-b epsilon-n age-max
                     global-error-decrease)
  ;; 3. find the nearest
  (define two-nearest (find-two-nearest data units))
  (define nearest (car two-nearest))
  (define second-nearest (cadr two-nearest))
  ;; 4. Find all emanating edges from nearest unit
  (define emanating-edges (unit-edges nearest))
  ;; 4. Increment the age of all the edges
  (for ([edge emanating-edges]) (increment-age! edge))
  ;; 5. Update local error
  (set-unit-error! nearest (euclidean-distance (unit-position nearest) data))
  ;; find neighbors
  (define neighbors (find-neighbors nearest emanating-edges))
  (for ([neighbor neighbors])
    (set-unit-error! neighbor
                     (euclidean-distance (unit-position neighbor) data)))
  ;; 6. Move a unit towards the input by two different fractions
  (move-by-a-fraction! nearest data epsilon-b)
  (for ([neighbor neighbors])
    (move-by-a-fraction! neighbor data epsilon-n))
  ;; 7. set the age of the edge to zero, or create if edge nonexistent
  (define edge-nearest-second-nearest (find-edge nearest
                                                 second-nearest))
  (if (empty? edge-nearest-second-nearest)
      ;; create a new edge
      (let ([new-edge (edge nearest second-nearest 0)])
        (set-unit-edges! nearest (cons new-edge (unit-edges nearest)))
        (set-unit-edges! second-nearest (cons new-edge (unit-edges
                                                        second-nearest))))
      ;; if edge exists, set its age to zero
      (when edge-nearest-second-nearest
        (set-edge-age! edge-nearest-second-nearest 0)))
  ;; 8. remove edges whose age is larger than a-max
  (for ([edge emanating-edges])
    (when (> (edge-age edge) age-max)
      (remove-edge! edge)))

  ;; 8. A unit has no emanating edges, remove it
  (set! units (leave-units-with-edges units))
  ;; 10. Decrease all error variables by multiplying them with a constant d
  (for ([unit units])
    (set-unit-error! unit (* (unit-error unit) global-error-decrease)))
  units)

(define (make-gng-network)
  (define units empty)
  (define initialized? #f)
  (define n-iterations 0)

  (define unit-insertion-counter 0)
  (define (increment-insertion-counter! limiter)
    (set! unit-insertion-counter (modulo (+ 1 unit-insertion-counter)
                                         limiter)))
  (define (run-gng units data
                   epsilon-b epsilon-n age-max global-error-decrease
                   unit-insertion-interval alpha n-max)
    (for ([d data])
      ;; sample data
      (set! units (gng-update! units d epsilon-b epsilon-n age-max
                               global-error-decrease))
      ;; 9. Insert edge when the unit-insertion-interval exceeded
      (when (and (< (length units) n-max) (zero? unit-insertion-counter))
        (set! units (insert-new-unit! units alpha)))
      (increment-insertion-counter! unit-insertion-interval))
    units)

  (λ (msg . args)
    (match msg
      ['reset
       (define dims (car args))
       (set! units (make-initial-network dims))
       (set! initialized? #f)
       (set! n-iterations 0)]
      ['run
       (define data (list-ref args 0))
       (unless initialized?
         (set-unit-position! (first units) (car data))
         (set-unit-position! (second units) (cadr data))
         (set! initialized? #t))
       (set! units (apply run-gng units args))
       (set! n-iterations (+ n-iterations (length data)))]
      ['get-units units]
      ['get-iterations n-iterations])))

(define (plot-2d-gng gng)
  (define units (gng 'get-units))
  (define n-iterations (gng 'get-iterations))
  (define n-nodes (length units))
  (define edges (remove-duplicates (flatten (map unit-edges units))))
  (plot-pict (list
              (for/list ([edge edges])
                (define posn1 (unit-position (edge-unit1 edge)))
                (define posn2 (unit-position (edge-unit2 edge)))
                (lines (list posn1 posn2)
                       #:color "gray"))
              (points (for/list ([unit units])
                        (unit-position unit))
                      #:sym 'fullcircle3
                      #:color "blue"
                      #:fill-color "blue"))
             #:title (~a n-iterations " iterations, " 
                         n-nodes " nodes")
             #:x-label ""
             #:y-label ""))

(define (run-and-plot gng data-fn n)
  (define data (for/list ([i n]) (data-fn)))
  (gng 'run data
       *epsilon-b* *epsilon-n* *age-max* *global-error-decrease*
       *unit-insertion-interval* *alpha* *n-max*)
  (plot-2d-gng gng))

;; random data function
(define (random-square-fn)
  (define displacement 0.0)
  (λ ()
    (define x (random 1000))
    (define y (random 1000))
    (flvector (+ displacement x)
              (+ displacement y))))

(plot (points
       (let ([square-fn (random-square-fn)])
         (for/list ([i 1000]) (square-fn)))))

(define *epsilon-b* 0.01) ;; movement fraction for the nearest
(define *epsilon-n* 0.005) ;; movement fraction for the neighbors of nearest
(define *age-max* 100) ;; delete an edge after its age is greater than age-max
(define *global-error-decrease* 0.995)
(define *unit-insertion-interval* 100)
(define *alpha* 0.5)
(define *n-max* 100)

(define gng (make-gng-network))
(gng 'reset 2)

(define image-columns 2)

(define data-fn0 (random-square-fn))

(plot-images image-columns
             (append (for/list ([i 6])
                       (run-and-plot gng data-fn0 500))
                     (list (run-and-plot gng data-fn0 7000))))
