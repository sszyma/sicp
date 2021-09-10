;; (cons a b) constructs a pair of nonnegative integers a, b, the result is a positive integer
(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car x)
  (define (iter count2s rest)
    (let ((next (/ rest 2)))
      (if (not (integer? next)) 
	  count2s
	  (iter (+ count2s 1) next))))
  (iter 0 x))

(define (cdr x)
  (define (iter count3s rest)
    (let ((next (/ rest 3)))
      (if (not (integer? next))
	  count3s
	  (iter (+ count3s 1) next))))
  (iter 0 x))

