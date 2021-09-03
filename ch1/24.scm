;; Use your search-for-primes  procedure to find the
;; three smallest primes larger than 1000; larger than
;; 10,000; larger than 100,000; larger than 1,000,000.
;; Note the time needed to test each prime. 

;; makes the timed-prime-test in range [a,b]
(define (search-for-primes a b)
  (cond ((> a b))
        ((fast-prime? a 1) (timed-prime-test a)
	                 (search-for-primes (+ a 1) b))
        (else (search-for-primes (+ a 1) b))))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (real-time)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 1) ;; once is enough
      (report-prime (- (real-time) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

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

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else #f)))

;; the CPUs are too fast now...
(display (search-for-primes 1000 1100)) ;; ~1ms
(newline)
(display (search-for-primes 10000 10100)) ;; ~3ms
(newline)
(display (search-for-primes 100000 100100)) ;; ~9ms
(newline)
(display (search-for-primes 1000000 1000100)) ;; ~26ms

