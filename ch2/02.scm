;; (make-segment p q) creates a segment in the Euclidean
;; plane with endpoints p, q
(define (make-segment p q) (cons p q))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

;; (make-point x y) creates a point (x,y) in the Euclidean plane
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (midpoint-segment s)
  (let ((p (start-segment s))
	(q (end-segment s)))
    (make-point (/ (+ (x-point p) (x-point q))
		   2)
		(/ (+ (y-point p) (y-point q))
		   2))))
