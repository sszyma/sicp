(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b))))

(define (factorial n)
  (if (= n 0)
      1
      (product (lambda (x) x)
	       1
	       (lambda (x) (+ x 1))
	       n)))

;; an approximation of pi
(define (pi n)
  (define (square x) (* x x))
  (define (term x) (/ (* 4 (square x))
		      (- (* 4 (square x)) 1)))
  (* 2 (product term 1.0 (lambda (x) (+ x 1)) n)))


;; iterative product
(define (product-iter term a next b)
  (define (iter i acc)
    (if (> a b)
	acc
	(iter (next i) (* (term i) acc))))
  (iter a 1))
