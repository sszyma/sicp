;; returns a list of the same elements in reverse order
(define (reverse list)
  (define (iter rest result)
    (if (null? rest)
	result
	(iter (cdr rest) (cons (car rest) result))))
  (iter list '()))
