;; fibonacci numbers in logarithmic time
;; note that b_next = bp' + aq'
;;                  = (bp + aq)p + (bq + aq + ap)q
;;                  = b(pq + q^2) + a(2pq + q^2)
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (define (square x) (* x x))
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p q) (square q))
                   (+ (* 2 p q) (square q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
