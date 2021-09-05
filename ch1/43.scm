;; computes f^n(x) for n>=0
(define (repeated f n)
  (define (compose f g)
    (lambda (x) (f (g x))))
  (if (= n 0)
      (lambda (x) x)
      (compose f (repeated f (- n 1)))))
