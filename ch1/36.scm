(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

;; a solution to the equation x^x = 1000 (takes 29 steps/approximations)
(newline)
(display 
  (fixed-point (lambda (x) (/ (log 1000) (log x))) 5))
(newline)
;; the same solution using average damping takes only 8 steps
(define (average x y) (/ (+ x y) 2))
(newline)
(display
 (fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 5))
(newline)
