;; computes the nth root of x
(define (nrt n x)
  (define tolerance 0.00001)
  (define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
      (< (abs (- v1 v2))
	 tolerance))
    (define (try guess)
      (let ((next (f guess)))
	(if (close-enough? guess next)
	    next
	    (try next))))
    (try first-guess))
  (define (average-damp f)
    (lambda (x) (/ (+ x (f x)) 2)))
  (define (repeated f n)
    (define (compose f g)
      (lambda (x) (f (g x))))
    (if (= n 0)
	(lambda (x) x)
	(compose f (repeated f (- n 1)))))
  ;; the number of damps increases with every power of 2
  (fixed-point ((repeated average-damp (floor (/ (log n) (log 2))))
		(lambda (y) (/ x (expt y (- n 1)))))
	       1.0))

