;; A Set is an unordered list with reoccurring elements
(define (element-of-set? x set)
  (cond ((null? set) #f)
	((equal? x (car set)) #t)
	(else (element-of-set? x (cdr set)))))

(define (adjoin-set x set) (cons x set))

(define (union-set set1 set2) (append set1 set2))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((element-of-set? (car set1) set2)
	 (intersection-set (cdr set1) set2))
	(else (cons (car set1)
		    (intersection-set (cdr set1) set2)))))

;; The change in the representation of sets to allow duplicate
;; elements increases the efficiency of the procedures adjoin-set
;; and union-set. Even though the definition of intersection-set
;; remains the same, the possibility that some element has a
;; duplicate in the front of the list potentially increases the
;; efficiency of this procedure. It would be less efficient than
;; originally, however, if a given set had many elements with
;; many duplicates.
