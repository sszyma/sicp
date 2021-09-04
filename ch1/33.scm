(define (filtered-accumulate combiner
			     filter
			     null-value
			     term
			     a
			     next
			     b)
  (if (> a b)
      null-value
      (combiner (if (filter a)
		    (term a)
		    null-value)
		(filtered-accumulate combiner
				     filter
				     null-value
				     term
				     (next a)
				     next
				     b))))

(define (sum-sqr-primes a b)
  (define (square x) (* x x))
  (define (prime? n)      
    (define (smallest-divisor n)
      (define (find-divisor n test-divisor)
	(cond ((> (square test-divisor) n) n)
              ((divides? test-divisor n) test-divisor)
              (else (find-divisor n (+ test-divisor 1)))))
      
      (find-divisor n 2))
    (define (divides? a b) (= (remainder b a) 0))    
    (if (= n 1) #f
	(= (smallest-divisor n) n)))
  (filtered-accumulate +
		       prime?
		       0
		       square
		       a
		       (lambda (x) (+ x 1))
		       b))

;; the product of all natural numbers < n,
;; that are relatively prime with respect to n
(define (prod-rel-prime n)
  (define (gcd a b)
    (if (= b 0)
	a
	(gcd b (remainder a b))))
  (filtered-accumulate *
		       (lambda (x) (= (gcd n x) 1))
		       1
		       (lambda (x) x)
		       1
		       (lambda (x) (+ x 1))
		       (- n 1)))


