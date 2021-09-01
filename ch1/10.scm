;; Ackermann's function
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;; The values of three expressions are

(A 1 10)
;; (A 0 (A 1 9))
;; (* 2 (A 1 9))
;; (* 2 (* 2 (A 1 8)))
;; ...
;; 2^10 = 1024

(A 2 4)
;; (A 1 (A 2 3))
;; (A 1 (A 1 (A 2 2)))
;; (A 1 (A 1 (A 1 (A 2 1))))
;; (A 1 (A 1 (A 1 2)))
;; (A 1 (A 1 4))
;; (A 1 16)
;; 2^16

(A 3 3)
;; (A 2 (A 3 2))
;; (A 2 (A 2 (A 3 1)))
;; (A 2 (A 2 2))
;; (A 2 4)
;; 2^16

;; Some functions defined in terms of the Ackermann's function
;; and their mathematical definitions

;; f(n) = 2n
(define (f n) (A 0 n))

;; g(n) = 2^n
(define (g n) (A 1 n))

;; h(n) = 2^2^2^2^...^2 (n times)
(define (h n) (A 2 n))
