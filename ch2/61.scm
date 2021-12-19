;; Set is an ordered list with a given order.
;; Below we consider only the sets of numbers
;; listed in increasing order.

;; Number [Set-of Number] -> Boolean
(define (element-of-set? x set)
  (cond ((null? set) #f)
	((= x (car set)) #t)
	((< x (car set)) #f)
	(else (element-of-set? x (cdr set)))))

;; [Set-of Number] [Set-of Number] -> [Set-of Number]
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
	(cond ((= x1 x2)
	       (cons x1
		     (intersection-set (cdr set1)
				       (cdr set2))))
	      ((< x1 x2)
	       (intersection-set (cdr set1) set2))
	      ((> x1 x2)
	       (intersection-set set1 (cdr set2)))))))

;; Number [Set-of Number] -> [Set-of Number]
;; If the number x we are to adjoin is less than the
;; first element of the ordered set, or if the set
;; is empty, then we know that x is not in this set.
(define (adjoin-set x set)
  (if (or (null? set)
	  (< x (car set))
	  (= x (car set)))
      (cons x set)
      (cons (car set)
	    (adjoin-set x (cdr set)))))
