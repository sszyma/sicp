;; (make-rat n d) returns a rational number with numerator n and denominator d
(define (make-rat n d)
  (define (gcd a b)
    (if (= b 0)
	a
	(gcd b (remainder a b))))
  (let ((g (gcd (abs n) (abs d))))
    (let ((n1 (/ n g))
	  (d1 (/ d g)))
      (if (negative? d1)
	  (cons (- n1) (- d1))
	  (cons n1 d1)))))

;; (numer x) returns the numerator of the rational number x
(define (numer x) (car x))

;; (denom x) returns the denominator of the rational number x
(define (denom x) (cdr x))

;; (equal-rat? x y) checks whether two rational numbers x and y are equal
(define (equal-rat? x y)
  (and (= (numer x) (numer y))
       (= (denom x) (denom y))))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))
