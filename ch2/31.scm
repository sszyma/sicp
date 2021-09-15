;; applies proc to every leaf of a tree
(define (tree-map proc tree)
  (cond ((null? tree) '())
      ((not (list? tree))
       (proc tree))
      (else
       (cons (tree-map proc (car tree))
	     (tree-map proc (cdr tree))))))

(define (square-tree tree)
  (tree-map (lambda (x) (* x x)) tree))
