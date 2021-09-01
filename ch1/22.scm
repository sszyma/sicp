;; Use your search-for-primes  procedure to find the
;; three smallest primes larger than 1000; larger than
;; 10,000; larger than 100,000; larger than 1,000,000.
;; Note the time needed to test each prime. 

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (real-time)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (real-time) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (smallest-divisor n)
  (define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
  (find-divisor n 2))

(define (divides? a b) (= (remainder b a) 0))

(define (square x) (* x x))

;; makes the timed-prime-test in range [a,b]
(define (search-for-primes a b)
  (cond ((> a b))
        ((prime? a) (timed-prime-test a)
                    (search-for-primes (+ a 1) b))
        (else (search-for-primes (+ a 1) b))))



;; they program does run approximately 3 times longer as the
;; argument increases 10 times, just as expected (at least in Racket)
(display (search-for-primes 1000 1100)) ;; ~1ms
(newline)
(display (search-for-primes 10000 10100)) ;; ~3ms
(newline)
(display (search-for-primes 100000 100100)) ;; ~9ms
(newline)
(display (search-for-primes 1000000 1000100)) ;; ~26ms


