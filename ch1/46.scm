;; iterative improvement abstraction
(define (iterative-improve good-enough? improve guess)
  (let ((next (improve guess)))
    (if (good-enough? guess next)
	next
	(iterative-improve good-enough? improve next))))

(define (fixed-point f guess)
  (iterative-improve (lambda (g0 g1) (< (abs (- g0 g1)) tolerance))
		     f
		     guess))
(define tolerance 0.00001)

(define (sqrt x)
  (define (square x) (* x x))
  (iterative-improve (lambda (g0 g1) (< (abs (- (square g0) x)) tolerance))
		     (lambda (g) (/ (+ g (/ x g)) 2))
		     1.0))

		     
