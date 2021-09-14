(load "12.scm")

(define A (make-center-percent 20 0.23))
(define B (make-center-percent 83 0.01))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
		(add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one (add-interval (div-interval one r1)
		       (div-interval one r2)))))

;; additional arithmetic operations on inexact quantities
;; increase the uncertainty of the result
(display (par1 A B))
(newline)
(display (par2 A B))
(newline)
(display (div-interval A A))
(newline)
(display (div-interval A B))
(newline)
(display (mul-interval B (div-interval A B)))
(newline)
