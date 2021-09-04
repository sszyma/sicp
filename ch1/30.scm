;; iterative sum procedure
(define (sum term a next b)
  (define (iter i acc)
    (if (> i b)
	acc
	(iter (next i) (+ (term i) acc))))
  (iter a 0))
