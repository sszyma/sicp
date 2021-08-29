#lang sicp

;; iterative repeated multiplication in a logarithmic time
(define (* a b)
  (define (double x) (+ x x))
  (define (even? x) (= (remainder x 2) 0))
  (define (iter a b acc)
    (cond ((= b 0) acc)
          ((even? b) (iter (double a) (/ b 2) acc))
          (else (iter a (- b 1) (+ a acc)))))
  (iter a b 0))
