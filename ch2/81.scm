(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (if (= (length args) 2)
	      (let ((type1 (car type-tags))
		    (type2 (cadr type-tags))
		    (a1 (car args))
		    (a2 (cadr args)))
		(if (eq? type1 type2)
		    (error "No method for this type" "APPLY-GENERIC"
			   (list op type-tags))
		    (let ((t1->t2 (get-coercion type1 type2))
			  (t2->t1 (get-coercion type2 type1)))
		      (cond (t1->t2
			     (apply-generic op (t1->t2 a1) a2))
			    (t2->t1
			     (apply-generic op a1 (t2->t1 a2)))
			    (else
			     (error
			      "No method for these types" "APPLY-GENERIC"
			      (list op type-tags)))))))
	      (error "No method for these types" "APPLY-GENERIC"
		     (list op type-tags)))))))

;; a)
;; If apply generic is called with two arguments of a type for an operation
;; that is not found in the table for those types then it tries to coerce
;; the two arguments to each others type, and succeeds if the procedures that
;; coerce a type to itself has been installed, but then outputs an error
;; since the corresponding operation still has not been defined for their
;; type.

;; b)
;; ... Thus nothing has to be done about coercion of arguments of the
;; same type.

;; c) apply-generic has been modified above
