(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (procedure term a next b)
  (accumulate * 1 term a next b))

;; an iterative version
(define (accumulate-iter combiner null-value term a next b)
  (define (iter i acc)
    (if (> i b)
	acc
	(iter (next i) (combiner (term i) acc))))
  (iter a null-value))
