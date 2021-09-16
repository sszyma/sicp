(define (square-list list)
  (if (null? list)
      list
      (cons ((lambda (x) (* x x)) (car list))
	    (square-list (cdr list)))))

(define (squarelist list)
  (map (lambda (x) (* x x)) list))
