;; for compatibility
(define (make-point x y) (cons x y))

;; (make-vec x y) creates a vector [x,y] 
(define (make-vec x y) (cons x y))
(define (x-vec p) (car p))
(define (y-vec p) (cdr p))

(define (magnitude v)
  (define (square x) (* x x))
  (sqrt (+ (square (x-vec v)) (square (y-vec v)))))

(define (add-vec v w)
  (make-vec (+ (x-vec v) (x-vec w))
	    (+ (y-vec v) (y-vec w))))

(define (sub-vec v w)
  (make-vec (- (x-vec v) (x-vec w))
	    (- (y-vec v) (y-vec w))))

(define (dot-product v w)
  (+ (* (x-point v) (x-point w))
     (* (y-point v) (y-point w))))

(define (perpendicular? v w)
  (= (dot-product v w) 0))

;; (make-rec1 v w) creates a rectangle with sides represented by vectors v and w, if possible
(define (make-rec1 v w)
  (if (perpendicular? v w)
      (cons v w)
      (error "MAKE-REC1" "THE VECTORS ARE NOT ORTHOGONAL" v w)))

;; (make-rec2 p q r) creates a rectangle with points p, q, r as vertices, if possible
(define (make-rec2 p q r)
  (let ((v1 (sub-vec p q))
	(v2 (sub-vec p r))
	(v3 (sub-vec q r)))
    (cond ((perpendicular? v1 v2) (cons v1 v2))
	  ((perpendicular? v1 v3) (cons v1 v3))
	  ((perpendicular? v2 v3) (cons v2 v3))
	  (else (error "MAKE-REC2"
		       "CANNOT CONSTRUCT A RECTANGLE WITH SUCH VERTICES" p q r)))))

;; technically speaking the selectors are the same, and so must be the representations of the
;; rectangle, but I cannot think of any other reasonable way to represent them
(define (length r) (car r))
(define (width r) (cdr r))

(define (perimeter r)
  (* 2 (+ (magnitude (width r))
	  (magnitude (length r)))))

(define (area r)
  (* (magnitude (width r))
     (magnitude (length r))))
