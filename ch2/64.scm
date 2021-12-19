(load "63.scm")

;; [Ordered list] -> BTree
(define (list->tree elements)
  ;; OList Number -> [cons BTree OList]
  (define (partial-tree elts n)
    (if (= n 0)
	(cons '() elts)
	(let ((left-size (quotient (- n 1) 2)))
	  (let ((left-result (partial-tree elts left-size)))
	    (let ((left-tree (car left-result))
		  (non-left-elts (cdr left-result))
		  (right-size (- n (+ left-size 1))))
	      (let ((this-entry (car non-left-elts))
		    (right-result (partial-tree (cdr non-left-elts)
						right-size)))
		(let ((right-tree (car right-result))
		      (remaining-elts (cdr right-result)))
		  (cons (make-tree this-entry left-tree right-tree)
			remaining-elts))))))))
  (car (partial-tree elements (length elements))))

;; The partial tree procedure takes an ordered list and a number
;; representing the number of the first elements in the list that
;; are about to be included in the resulting BTree. The resulting
;; Btree having n elements has the first a = (floor (/ (- n 1) 2))
;; elements in the left branch, one element for the entry note, and
;; hence b = (- n a 1) elements for the right branch. To produce a
;; BTree we produce a Btree of the left-branch with the first a elements,
;; take the first unused element as the entry node, and take the first b
;; elements from the remaining unused elements for the Btree of the
;; right-branch. The unused elements are paired with the resulting Btree.
;; If the number of the elements in a given list to be included in the
;; BTree is the same as the number of the elements in a given list, then
;; all the elements will be used.

;; Sample result for (list->tree '(1 3 5 7 9 11))
(define A (make-tree 5
		     (make-tree 1
				'()
				(make-tree 3 '() '()))
		     (make-tree 9
				(make-tree 7 '() '())
				(make-tree 11 '() '()))))

;; To make a BTree of n elements we must make two BTrees of about n/2 elements,
;; so the list->tree procedure has an O(n) order of growth.



