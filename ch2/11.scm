;; creates an interval [a,b], a<=b
(define (make-interval a b) (cons a b))
(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))

(define (mul-interval x y)
  (let ((lx (lower-bound x))
	(ux (upper-bound x))
	(ly (lower-bound y))
	(uy (upper-bound y)))
    (cond ((and (>= lx 0) (>= ly 0))
	   (make-interval (* lx ly) (* ux uy)))
	  ((and (<= ux 0) (<= uy 0))
	   (make-interval (* ux uy) (* lx ly)))
	  ((and (>= lx 0) (<= uy 0))
	   (make-interval (* ux ly) (* lx uy)))
	  ((and (<= ux 0) (>= ly 0))
	   (make-interval (* lx uy) (* ux ly)))
	  ((and (<= lx 0) (>= ux 0))
	   (cond ((and (<= ly 0) (>= uy 0))
		  (make-interval (min (* lx uy) (* ly ux))
				 (max (* ux uy) (* ly lx))))
		 ((and (<= ly 0) (<= uy 0))
		  (make-interval (* ly ux) (* lx ly)))
		 (else (make-interval (* lx uy) (* ux uy)))))
	  (else
	   (if (and (>= lx 0) (>= ux 0))
	       (make-interval (* ly ux) (* uy ux))
	       (make-interval (* lx uy) (* ly lx)))))))

