;; computes the binomial coefficient for 0 <= k <= n
(define (binomial n k)
  (cond ((= n 0) 1)
        ((or (= n k) (= k 0)) 1)
        (else (+ (binomial (- n 1) k)
                 (binomial (- n 1) (- k 1))))))
