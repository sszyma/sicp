;; Tree -> [List-of X]
;; returns a list of tree nodes in left-to-right order
(define (fringe tree)
  (cond ((null? tree) '())
	((not (list? (car tree)))
	 (cons (car tree) (fringe (cdr tree))))
	(else (append (fringe (car tree))
		      (fringe (cdr tree))))))
