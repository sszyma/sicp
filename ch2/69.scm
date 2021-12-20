(load "68.scm")

;; [List-of Leaf] -> Huffman Tree
(define (generate-huffman-tree pairs)
  (define (successive-merge pairs)
    (if (null? (cdr pairs))
	(car pairs)
	(successive-merge
	 (adjoin-set
	  (make-code-tree (car pairs)
			  (cadr pairs))
	  (cddr pairs)))))
  (successive-merge (make-leaf-set pairs)))
  
(define A '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1)))


