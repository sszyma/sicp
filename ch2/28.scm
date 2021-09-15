;; Tree -> [List-of X]
;; returns a list of tree nodes in left-to-right order
(define (fringe tree)
  (cond ((null? tree) '())
	((not (list? tree))
	 (list tree))
	(else (append (fringe (car tree))
		      (fringe (cdr tree))))))
