;; numerical integration via Simpson's rule
(define (integral f a b n)
  (let ((h (/ (- b a) n)))
    (define (sum term a next b)
      (if (> a b)
	  0
	  (+ (term a)
	     (sum term (next a) next b))))
    (define (y k) (f (+ a (* k h))))
    (define (next x) (+ x 2))
    (* (+ (y 0)
	  (* 2 (sum y 2 next (- n 2)))
	  (* 4 (sum y 1 next (- n 1)))
	  (y n))
       (/ h 3.0))))



