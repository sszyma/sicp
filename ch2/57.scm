(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (variable? exp) (symbol? exp))
(define (same-variable? v1 v2) (eq? v1 v2))

;; The output now is back to being ugly, but I suppose that
;; we could treat each expression as a tree and remove unnecessary
;; elements of each list and sublist depending on '+/'* sign. The
;; problem is what if the sublist is e.g. ('* 1 x). If I remove 1
;; '(* x) would not be valid.
(define (make-sum a1 a2 . rest)
  (let ((all (cons a1 (cons a2 rest))))
    (let ((nums (filter number? all)))
      (let ((num (fold-right + 0 nums)))
	(if (= (length nums) (length all))
	    num
	    (cons '+ (cons num (filter (lambda (x) (not (number? x))) all))))))))
(define (sum? exp) (and (list? exp) (eq? (car exp) '+)))
(define (addend sum) (cadr sum))
(define (augend sum)
  (let ((rest (cddr sum)))
    (if (null? (cdr rest))
	(car rest)
	(cons '+ rest))))

(define (negate exp)
  (if (number? exp)
      (- exp)
      (list '- exp)))
(define (negation? exp)
  (and (list? exp) (eq? (car exp) '-) (null? (cddr exp))))
(define (negated exp) (cadr exp))

(define (make-difference m s) ;; just do it
  (cond ((=number? m 0) (negate s))
	((=number? s 0) (negate m))
	((and (number? m) (number? s)) (- m s))
	(else (list '- m s))))
(define (difference? exp)
  (and (list? exp) (eq? (car exp) '-)))
(define (minuend d) (cadr d))
(define (subtrahend d) (caddr d))

(define (make-product m1 m2 . rest)
  (let ((all (cons m1 (cons m2 rest))))
    (let ((nums (filter number? all)))
      (let ((num (fold-right * 1 nums)))
	(if (= (length nums) (length all))
	    num
	    (cons '* (cons num (filter (lambda (x) (not (number? x))) all))))))))
(define (product? exp) (and (list? exp) (eq? (car exp) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p)
  (let ((rest (cddr p)))
    (if (null? (cdr rest))
	(car rest)
	(cons '* rest))))


(define (make-quotient d1 d2)
  (cond ((=number? d2 0)
	 (error "MAKE-QUOTIENT" "The divisor is zero" d2))
	((=number? d2 1) d1)
	((and (number? d1) (number? d2)) (/ d1 d2))
	(else (list '/ d1 d2))))
(define (quotient? exp)
  (and (list? exp) (eq? (car exp) '/)))
(define (dividend q) (cadr q))
(define (divisor q) (caddr q))

(define (make-exponentiation base exponent)
  (cond ((or (=number? base 0)
	     (=number? base 1)
	     (=number? exponent 1)) base)
	((=number? exponent 0) 1)
	((and (number? base) (number? exponent))
	 (expt base exponent))
	(else (list '** base exponent))))
(define (exponentiation? exp)
  (and (list? exp) (eq? (car exp) '**)))
(define (base expt) (cadr expt))
(define (exponent expt) (caddr expt))

(define (make-log antilog)
  (if (number? antilog)
      (log antilog)
      (list 'log antilog)))
(define (log? exp)
  (and (list? exp) (eq? (car exp) 'log)))
(define (antilog log) (cadr log))

(define euler (exp 1))

;; It's still ugly as hell, so I no longer have the
;; motivation to implement trig functions
(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((negation? exp)
	 (negate (deriv (negated exp) var)))
	((difference? exp)
	 (make-difference (deriv (minuend exp) var)
			  (deriv (subtrahend exp) var)))
	((product? exp)
	 (let ((plier (multiplier exp))
	       (plicand (multiplicand exp)))
	   (make-sum
	    (make-product plier (deriv plicand var))
	    (make-product (deriv plier var) plicand))))
	((quotient? exp)
	 (let ((dividend (dividend exp))
	       (divisor (divisor exp)))
	   (make-quotient
	    (make-difference
	     (make-product (deriv dividend var) divisor)
	     (make-product dividend (deriv divisor var)))
	    (make-exponentiation divisor 2))))
	((exponentiation? exp)
	 (let ((base (base exp))
	       (expt (exponent exp)))
	   (let ((logbase (make-log base)))
	     (make-product
	      (make-exponentiation euler (make-product expt logbase))
	      (deriv (make-product expt logbase) var)))))
	((log? exp)
	 (let ((antilog (antilog exp)))
	   (make-quotient (deriv antilog var) antilog)))
	(else
	 (error "DERIV" "Unknown expression type" exp))))

