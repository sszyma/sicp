(define (expmod base exp m)
    (define (square x) (* x x))
    (cond ((= exp 0) 1)
	  ((even? exp)
	   (remainder
	    (square (expmod base (/ exp 2) m))
	    m))
	  (else
	   (remainder
	    (* base (expmod base (- exp 1) m))
	    m))))


(define (fast-prime? n)
  (define (try-it a)
    (cond ((= a 1) #t)
	  ((not (= (expmod a n n) a)) #f)
	  (else (try-it (- a 1)))))
  (try-it (- n 1)))

;; the four smallest carmichalean numbers that fool the fermat test for
;; all 1 < a < n.
(display (fast-prime? 561))
(display (fast-prime? 1105))
(display (fast-prime? 1105))
(display (fast-prime? 1729))
	  
