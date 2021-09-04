(define (cont-frac n d k)
  (define (helper i)
    (if (= i k)
	0
	(/ (n i) (+ (d i)
		    (helper (+ i 1))))))
  (helper 1))

;; an approximation of e using Euler's continued fractions
(define e
  (+ 2 (cont-frac (lambda (x) 1.0)
		  (lambda (x) (if (= (remainder x 3) 2)
				  (* 2/3 (+ x 1))
				  1.0))
		  20)))
;; I have no idea why it gives incorrect results
