(define (cont-frac n d k)
  (define (helper i)
    (if (= i k)
	0
	(/ (n i) (+ (d i)
		    (helper (+ i 1))))))
  (helper 1))

;; an iterative version 
(define (bottom-up n d k)
  (define (iter count acc)
    (if (= count 0)
	acc
	(iter (- count 1)
	      (/ (n k) (+ (d k) acc)))))
  (iter k 1))

;; an approximation of psi (which should be negative btw) to 4 decimal places
(define psi (cont-frac (lambda (x) 1.0) (lambda (x) 1.0) 12))
