(define (count-leaves t)
  (accumulate + 0 (map (lambda (b)
			 (if (list? b)
			     (count-leaves b)
			     1))
		       t)))
