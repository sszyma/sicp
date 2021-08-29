#lang sicp

;; (define (fast-expt b n)
;;   (cond ((= n 0) 1)
;;         ((even? n) (square (fast-expt b (/ n 2))))
;;         (else (* b (fast-expt b (- n 1))))))

;; let x be the recursion level
;; a/2^x <= 1
;; a <= 2^x
;; x >= log_2(a)
;; So T(n) = O(log_2(n)) = O(logn/log2) = O(logn)

;; an iterative version
(define (fast-expt b n)
  (define (even? n)
    (= (remainder n 2) 0))
  (define (square x) (* x x))
  (define (iter b n acc)
    (cond ((= n 0) acc)
          ((even? n) (iter (square b) (/ n 2) acc))
          (else (iter b (- n 1) (* b acc)))))
  (iter b n 1))
