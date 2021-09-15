;; A Mobile is a [List Branch Branch]
(define (make-mobile left right) (list left right))

;; A Branch is a [List Number [Mobile or Number]]
(define (make-branch length structure)
  (list length structure))

;; a)
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))

(define (branch-length mobile) (car mobile))
(define (branch-structure mobile) (cadr mobile))

;; b)
;; Mobile -> Number
(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

;; Mobile -> Number
(define (branch-weight branch)
    (let ((struct (branch-structure branch)))
      (if (number? struct)
	  struct
	  (total-weight struct))))

;; c)
;; Mobile -> Boolean
(define (balanced? mobile)
  (let ((left (left-branch mobile))
	(right (right-branch mobile)))
    (and (= (* (branch-length left) (branch-weight left))
	    (* (branch-length right) (branch-weight right)))
	 (balanced-subweight? left)
	 (balanced-subweight? right))))

(define (balanced-subweight? branch)
  (let ((struct (branch-structure branch)))
    (if (number? struct)
	#t
	(balanced? struct))))

;; d) if we change the representation of mobiles from lists to pairs,
;; there is nothing to change since every procedure is defined in terms of
;; selectors, assuming that the structure of a branch still must be either a
;; number or another mobile.
