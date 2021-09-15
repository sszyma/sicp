;; square-tree implemented directly
(define (square-tree tree)
  (cond ((null? tree) '())
	((not (list? tree))
	 ((lambda (x) (* x x)) tree))
	(else
	 (cons (square-tree (car tree))
	       (square-tree (cdr tree))))))

;; square-tree implemeneted using higher-order procedures
(define (square-tree-map tree)
  (map (lambda (b) (if (list? b)
		       (square-tree-map b)
		       ((lambda (x) (* x x)) b)))
       tree))
