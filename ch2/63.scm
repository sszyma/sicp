;; A Tree is a a list containing an Entry node, which is a number,
;; and left and right branches which are also Trees. We require all
;; elements in the left branch to be less than the entry node, and
;; all elements in the right branch to be greater than the entry node.

;; Number Tree Tree -> Tree
(define (make-tree entry left-branch right-branch)
  (list entry left-branch right-branch))

;; Tree -> Number
(define (entry tree) (car tree))
;; Tree -> Tree
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

;; Below an ordered set of numbers is represented as a Tree

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
	      (cons (entry tree)
		    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
	result-list
	(copy-to-list (left-branch tree)
		      (cons (entry tree)
			    (copy-to-list (right-branch tree)
					  result-list)))))
  (copy-to-list tree '()))

;; The result of the two procedures is exactly the same for every tree,
;; since both of them produce the previous representation of sets as
;; ordered lists, where the numbers are listed in increasing order.

(define A (make-tree 7
		     (make-tree 3
				(make-tree 1 '() '())
				(make-tree 5 '() '()))
		     (make-tree 9
				'()
				(make-tree 11 '() '()))))

(define B (make-tree 3
		     (make-tree 1 '() '())
		     (make-tree 7
				(make-tree 5 '() '())
				(make-tree 9
					   '()
					   (make-tree 11 '() '())))))

(define C (make-tree 5
		     (make-tree 3
				(make-tree 1 '() '())
				'())
		     (make-tree 9
				(make-tree 7 '() '())
				(make-tree 11 '() '()))))

(display (tree->list-1 A))
(newline)
(display (tree->list-1 B))
(newline)
(display (tree->list-1 C))
(newline)
(newline)
(display (tree->list-2 A))
(newline)
(display (tree->list-2 B))
(newline)
(display (tree->list-2 C))
(newline)

;; When evaluating balanced trees, the first procedure generates a process with an
;; O(n^2) order of growth, due to the append procedure which reitarates over the
;; left branch every time two additional elements are added to the list being appended.
;; On the other hand the second procedure generates an O(n) process since it "conses"
;; through the whole tree without performing redundant operations.

