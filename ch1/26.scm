(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (* (expmod base (/ exp 2) m)
		       (expmod base (/ exp 2) m))
		    m))
	(else
	 (remainder (* base
		       (expmod base (- exp 1) m))
		    m))))

;; The original expmod procedure made use of the
;; applicative-order evaluation and auxiliary square
;; procedure, among others, to reduce the running
;; time to O(logn), while this procedure by using
;; ordinary multiplication possibly generates
;; two recursive calls at every iteration, transforming
;; O(logn) process to O(log2^n) = O(nlog2) = O(n) process.
