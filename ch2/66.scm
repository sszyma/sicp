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

;; A Record is a list whose first element is a number, a Key.
;; A [Set-of Records] is a Tree of records ordered by their Keys

;; Key [Set-of Records] -> [or Record #False]
(define (lookup k sor)
  (cond ((null? sor) #f)
	((= k (key (entry sor)))
	 (entry sor))
	((< k (key (entry sor)))
	 (lookup k (left-branch sor)))
	((> k (key (entry sor)))
	 (lookup k (right-branch sor)))))
