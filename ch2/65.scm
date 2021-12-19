(load "61.scm")
(load "62.scm")
(load "64.scm")

(define (union-set-btree tree1 tree2) 
  (list->tree
   (union-set (tree->list-2 tree1) 
              (tree->list-2 tree2))))


(define (intersection-set-btree tree1 tree1)
  (list->tree
   (intersection-set (tree->list-2 tree1)
		     (tree->list-2 tree2))))
