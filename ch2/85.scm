(define (install-raise-package)
  (define (integer->rational n) (make-rational n 1))
  (define (rational->schemenum r) (exact->inexact r))
  (define (schemenum->complex x) (make-from-real-imag x 0))
  (put-coercion 'integer 'rational integer->rational)
  (put-coercion 'rational 'scheme-number rational->schemenum)
  (put-coercion 'scheme-number 'complex schemenum->complex)
  'done)

(define (install-project-package)
  (put-coercion 'complex 'real real-part)
  (put-coercion 'real 'rational exact->inexact)
  (put-coercion 'rational 'integer round))


;; To raise an object it suffices to maintain a raise package that
;; contains the procedures that coerce a type to the next one in the
;; tower and a TOWER itself which is an ordered list specifying
;; what a given type can be raised to. In other words, the higher level
;; in the TOWER, the further in the list.
(define (raise x)
  (let ((tag (type-tag x)))
    (let ((target (memq tag TOWER)))
      (if target
	  (if (null? (cdr target))
	      (error "Cannot raise this type" "RAISE" (list tag TOWER))
	      ((get-coercion tag (cadr target)) (contents x)))
	  (error "Cannot raise this type" "RAISE" (list tag TOWER))))))

;; Projection is raising but on the TOWER reversed; see ANTITOWER below
(define (project x)
  (let ((tag (type-tag x)))
    (let ((target (memq tag ANTITOWER)))
      (if target
	  (if (null? (cdr target))
	      #f
	      ((get-coercion tag (cadr target)) (contents x)))
	  #f))))

(define TOWER '(integer rational real complex))
(define ANTITOWER (reverse TOWER))

;; Let <t be a relation on the TOWER such that x <t y iff y is higher in
;; the TOWER than x. This is equivalent to the condition that y is contained
;; in (cdr (memq x TOWER)):
(define (<t x y)
  (let ((xisin (memq x TOWER))
	(yisin (memq y TOWER)))
    (if (and xisin yisin)
	(if (memq y (cdr xisin))
	    #t
	    #f)
	(error "These types are not in the TOWER"
	       "<t"
	       (list x y TOWER)))))

(define (raise-to x type)
  (if (eq? (tag-type x) type)
      x
      (raise-to (raise x) type)))

(define (drop x)
  (let ((candidate (project x)))
    ;; equ? predicate below is taken from the Problem 2.79
    ;; it is supposed to be modified so that it doesn't rely
    ;; on apply-generic 
    (if (and candidate (equ? x (raise candidate)))
	(drop candidate)
	x)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (let ((result (apply proc (map contents args))))
	    ;; We use it to test whether the result has a type; if it does, we drop it.
	    ;; Here I also permit myself to modify type-tag, so that if an object does not
	    ;; have a type, then type-tag outputs #f, not an error (which was rarely useful).
	    (if (type-tag result)
		(drop result)
		result))
	  (if (= (length args) 2)
	      (let ((type1 (car type-tags))
		    (type2 (cadr type-tags))
		    (a1 (car args))
		    (a2 (cadr args)))
		(cond ((<t type1 type2)
		       (apply-generic op (raise-to a1 type2) a2))
		      ((<t type2 type1)
		       (apply-generic op a1 (raise-to a2 type1)))
		      (else
		       (error
			"No method for these types" "APPLY-GENERIC"
			(list op type-tags)))))
	      (error "No method for these types" "APPLY-GENERIC"
		     (list op type-tags)))))))
