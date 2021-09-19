(define (cont-frac n d k)
  (define (helper i)
    (if (= i k)
	0
	(/ (n i) (+ (d i)
		    (helper (+ i 1))))))
  (helper 1))

(define (tan-cf x k)
  (cont-frac (lambda (n) (if (= n 1) (- x) (- x^2)))
	     (lambda (d) (- (* 2 d) 1))
	     k))
