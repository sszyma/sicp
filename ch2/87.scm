(load "ch2/78.scm")

;; I include here solutions to exercises 87, 88 and 89

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? x y) (eq? x y))

  ;; representation of terms and term lists for dense polys (ex. 89)
  (define (the-empty-termlist) '())
  (define (empty-termlist? l) (null? l))
  (define (first-term l) (car l))
  (define (rest-terms l) (cdr l))
  (define (adjoin-term t l) (cons t l))
  (define (make-term order coeff) (list order coeff))
  (define (order t) (car t))
  (define (coeff t) (cadr t))
  
  ;; operations on polys
  (define (add-poly f g)
    (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
	  ((empty-termlist? L2) L1)
	  (else
	   (let ((t1 (first-term L1)) (t2 (first-term L2)))
	     (cond ((> (order t1) (order t2))
		    (adjoin-term 
		     t1 (add-terms (rest-terms L1) L2)))
		   ((< (order t1) (order t2))
		    (adjoin-term
		     t2 (add-terms L1 (rest-terms L2))))
		   (else
		    (adjoin-term
		     (make-term (order t1)
				(add (coeff t1) (coeff t2)))
		     (add-terms (rest-terms L1)
				(rest-terms L2)))))))))
    (if (same-variable? (variable f) (variable g))
	(make-poly (variable f)
		   (add-terms (term-list f) (term-list g)))
	(error "Polys not in same var" "ADD-POLY" (list f g))))

  ;; ex. 88
  (define (negate-poly f)
    (define (map-termlist op L)
      (if (empty-termlist? L)
	  L
	  (adjoin-term (op (first-term L))
		       (map-termlist op (rest-termlist L)))))
    (make-poly (variable f)
	       (map-termlist (lambda (t) (make-term (order t)
						    (negate (coeff t))))
			     (term-list f))))

  (define (sub-poly f g)
    (add-poly f (negate g)))
  
  (define (mul-poly f g)
    (define (mul-terms L1 L2)
      (if (empty-termlist? L1) ;; why not check both at once?
	  (the-empty-termlist)
	  (add-terms (mul-term-by-all-terms (first-term L1) L2)
		     (mul-terms (rest-terms L1) L2))))
    (define (mul-term-by-all-terms t1 L)
      (if (empty-termlist? L)
	  (the-empty-termlist)
	  (let ((t2 (first-term L)))
	    (adjoin-term
	     (make-term (+ (order t1) (order t2))
			(mul (coeff t1) (coeff t2)))
	     (mul-term-by-all-terms t1 (rest-terms L))))))
    (if (same-variable? (variable f) (variable g))
	(make-poly (variable f)
		   (mul-terms (term-list f) (term-list g)))
	(error "Polys not in same var" "MUL-POLY" (list f g))))

  ;; interface for the rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (f g) (tag (add-poly f g))))
  (put 'negate '(polynomial)
       (lambda (f) (tag (negate f))))
  (put 'sub '(polynomial polynomial)
       (lambda (f g) (tag (sub-poly f g))))
  (put 'mul '(polynomial polynomial)
       (lambda (f g) (tag (mul-poly f g))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(install-polynomial-package)

(define (make-poly var terms) ((get 'make 'polynomial) var terms))

(define (install-general-arith-package)
  ;; internal procedures
  ;; in case of rationals we make use of their
  ;; representation with the lowest terms.
  (define (=rat p q)
    (and (= (numer p) (numer q))
	 (= (denom p) (denom q))))
  (define (=complex z w)
    (and (= (imag-part z) (imag-part w))
	 (= (real-part z) (real-part w))))
  (define (zero?-schemenum x)
    (= x 0))
  (define (zero?-rat p)
    (= (numer p) 0))
  (define (zero?-complex z)
    (and (= (real-part z) 0)
	 (= (imag-part z) 0)))
  (define (zero?-poly p) ;; ex. 87
    (empty-termlist? (term-list p)))
  ;; interface for the rest of the system
  (put 'equ? '(scheme-number scheme-number) =)
  (put 'equ? '(rational rational) =rat)
  (put 'equ? '(complex complex) =complex)
  (put '=zero? '(scheme-number) zero?-schemenum)
  (put '=zero? '(rational) zero?-rat)
  (put '=zero? '(complex) zero?-complex)
  (put '=zero? '(poly) zero?-poly)
  'done)

(install-general-arith-package)

(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
