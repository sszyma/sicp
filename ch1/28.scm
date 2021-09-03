(define (expmod base exp m)
  (define (square x) (* x x))
  ;; returns 0 when identifies a non-trivial square root of 1 modulo m
  ;; otherwise returns the original number
  (define (maybe-nontrivial x)
    (if (and (not (or (= x 1) (= x (- m 1))))
	     (= (remainder (square x) m) 1))
	0
	(remainder (square x) m))) ;; sadly this computation is repeated twice
  (cond ((= exp 0) 1)
	((even? exp)
	 (maybe-nontrivial (expmod base (/ exp 2) m)))
	(else
	 (remainder
	  (* base (expmod base (- exp 1) m))
	  m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
	((miller-rabin-test n) (fast-prime? n (- times 1)))
	(else #f)))


(display (fast-prime? 198256 1))
(display (fast-prime? 7 1))
;; the four smallest carmichalean numbers that fool the fermat test for
;; all 1 < a < n.
(display (fast-prime? 561 1))
(display (fast-prime? 1105 1))
(display (fast-prime? 1105 1))
(display (fast-prime? 1729 1))

