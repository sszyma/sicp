;; applies proc to a list of elements and returns #t
(define (for-each proc list)
  (cond ((null? list) #t)
	(else
	 (proc (car list))
	 (for-each proc (cdr list)))))
