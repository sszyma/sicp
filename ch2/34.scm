;; Let Polynomial be a [List-of Number], that is a list of coefficients,
;; ordered by their corresponding terms with the lowest to the highest power.

;; Number Polynomial -> Number
;; Evaluates a polynomial at x using Horner's algorithm
(define (horner-eval x poly)
  (accumulate (lambda (first rest) (+ first (* x rest)))
	      0
	      poly))
