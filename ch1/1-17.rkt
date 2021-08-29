#lang sicp

;; repeated multiplication in a linear time
;; (define (* a b)
;;   (cond ((or (= a 0) (= b 0)) 0)
;;         ((= b 1) a)
;;         (else (+ a (* a (- b 1))))))

;; ... in a logarithmic time
(define (* a b)
  (define (even? x)
    (= (remainder x 2) 0))
  (define (double x) (+ x x))
  (cond ((or (= a 0) (= b 0)) 0)
        ((= b 1) a)
        ((even? b) (double (* a (/ b 2))))
        (else (+ a (* a (- b 1))))))
