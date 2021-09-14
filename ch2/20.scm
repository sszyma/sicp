;; Number [List-of Number] -> [List-of Number]
;; returns a list of numbers with the same even-odd parity as the
;; first argument
(define (same-parity x . y)
  (define parity?
    (if (even? x)
	even?
	odd?))
  (define (iter lon)
    (if (null? lon)
	lon
	(let ((first (car lon))
	      (rest (cdr lon)))
	  (if (parity? first)
	      (cons first (iter rest))
	      (iter rest)))))
  (cons x (iter y)))
		
			     
