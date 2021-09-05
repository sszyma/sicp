;; applies f twice
(define (double f)
  (lambda (x) (f (f x))))

;; if inc is a procedure that adds 1 to its argument, then
;; (((double (double double)) inc) 5)
;; is expanded as
;; (((double (lambda (x) (double (double x)))) inc) 5)
;; (((lambda (x) (double (double (double (double x))))) inc) 5)
;; which corresponds to inc applied 2^4 times or
;; (+ 5 16)
;; 21
