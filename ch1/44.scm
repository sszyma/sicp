;; generates n-fold smoothed function f for n>=0
(define (smooth f n)
  ;; smoothes a continuous function f
  (define (smoothie f)
    (lambda (x) (/ (+ (f (+ x dx))
		      (f x)
		      (f (- x dx)))
		   3)))
  ;; computes f^n(x) for n>=0
  (define (repeated f n)
    (define (compose f g)
      (lambda (x) (f (g x))))
    (if (= n 0)
	(lambda (x) x)
	(compose f (repeated f (- n 1)))))
  ((repeated smoothie n) f))


