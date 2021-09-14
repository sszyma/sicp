(load "12.scm")

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
		(add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one (add-interval (div-interval one r1)
		       (div-interval one r2)))))

;; par2 is is a better procedure since it performs fewer
;; arithmetic operations directly on inexact quantities,
;; that is, r1 and r2 are introduced only once.
