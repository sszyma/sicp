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
  (define (next n)
    (if (odd? n)
        (+ n 2)
        (+ n 1)))
  (define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
  (find-divisor n 2))

(define (divides? a b) (= (remainder b a) 0))

(define (square x) (* x x))

;; makes the timed-prime-test in range [a,b]
(define (search-for-primes a b)
  (cond ((> a b))
        ((prime? a) (timed-prime-test a)
                    (search-for-primes (+ a 1) b))
        (else (search-for-primes (+ a 1) b))))



;; In Racket, the improved smallest-divisor procedure
;; reduced the running times by approximately half,
;; just as expected, but I suppose the results are
;; still dependent on the resource consumption of the
;; underlying system, as can be noted by sudden increase
;; in the running times for few procedure applications
;; of timed-prime-test. Also users on the internet, note
;; that the next procedure, halves the number of steps
;; but introduces another if-statement which might be
;; the reason why the running time is not exactly halved.
(display (search-for-primes 1000 1100)) ;; ~1ms
(newline)
(display (search-for-primes 10000 10100)) ;; ~2ms
(newline)
(display (search-for-primes 100000 100100)) ;; ~5ms
(newline)
(display (search-for-primes 1000000 1000100)) ;; ~14ms
