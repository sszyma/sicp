(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))
(define (type-tag datum)
  (cond ((pair? datum) (car datum))
	((number? datum) 'scheme-number)
	(else (error "Bad tagged datum" "TYPE-TAG" datum))))
(define (contents datum)
  (cond ((pair? datum) (cdr datum))
	((number? datum) datum)
	(else (error "Bad tagged datum" "CONTENTS" datum))))


(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable
	     (assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record
		   (assoc key-2 (cdr subtable))))
	      (if record (cdr record) #f))
	    #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
	     (assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record
		   (assoc key-2 (cdr subtable))))
	      (if record
		  (set-cdr! record value)
		  (set-cdr! subtable
			    (cons (cons key-2 value)
				  (cdr subtable)))))
	    (set-cdr! local-table
		      (cons (list key-1 (cons key-2 value))
			    (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define (make-coercion-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable
	     (assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record
		   (assoc key-2 (cdr subtable))))
	      (if record (cdr record) false))
	    false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
	     (assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record
		   (assoc key-2 (cdr subtable))))
	      (if record
		  (set-cdr! record value)
		  (set-cdr! subtable
			    (cons (cons key-2 value)
				  (cdr subtable)))))
	    (set-cdr! local-table
		      (cons (list key-1 (cons key-2 value))
			    (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define operation-coercion-table (make-table))
(define get-coercion (operation-table 'lookup-proc))
(define put-coercion (operation-table 'insert-proc!))

;; IT WORKS!! (I hope) See the random rational and coercion packages below
(define (apply-generic op . args)
  ;; Generates a list of length (length args) consisting only of a given symbol type
  (define (type-list type)
    (fold-right (lambda (x rest) (cons type rest)) '() args))
  (let ((type-tags (map type-tag args))
	(arguments (map contents args)))
    ;; Can all the argument types can be coerced to a given type? If so output
    ;; the list of the corresponding coercion procedures.
    (define (coerces? type)
      (fold-right (lambda (t rest) (let ((t->type (get-coercion t type)))
				     (cond ((eq? t type)
					    (cons (lambda (arg) (attach-tag type arg)) rest))
					   (t->type
					    (cons t->type rest))
					   (else #f))))
		  '() type-tags))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc arguments)
	  (let (;; a list of types for which a procedure op exists
		(coercion-candidates 
		 (filter (lambda (t) (get op (type-list t))) type-tags)))
	    (let (;; pairs of coercion target types/sequences of target coercion procedures)
		  (coercion-targets
		   (filter (lambda (p) (cadr p))
			   (map (lambda (t) (cons t (coerces? t))) coercion-candidates))))
	      (if (null? coercion-targets)
		(error "No method for these types" "APPLY-GENERIC" (list op type-tags))
		;; sample coercion target type and the corresponding coercion procedures
		(let ((coercion-type (car (car coercion-targets)))
		      (coercion-proc-sequence (cdr (car coercion-targets))))
		  (let ((new-proc (get op (type-list coercion-type))))
		    (apply new-proc (map contents
					 (map (lambda (proc arg) (proc arg))
					      coercion-proc-sequence
					      arguments))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (install-scheme-number-package)
  (put 'add '(scheme-number scheme-number) +)
  (put 'sub '(scheme-number scheme-number) -)
  (put 'mul '(scheme-number scheme-number) *)
  (put 'div '(scheme-number scheme-number) /)
  'done)

(install-scheme-number-package)

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (define (gcd a b)
      (if (= b 0)
	  a
	  (gcd b (remainder a b))))
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
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

  ;; interface for the rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'numer '(rational) numer)
  (put 'denom '(rational) denom)
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (numer r) (apply-generic 'numer r))
(define (denom r) (apply-generic 'denom r))

(install-rational-package)


(define (make-rational n d)
  ((get 'make 'rational) n d))


(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))


(define (install-random-coercion-package)
  (define (schemenum->rational n) (make-rational n 1))
  (put-coercion 'scheme-number 'rational schemenum->rational)
  'done)

(install-random-coercion-package)

(define (install-random-rational-package)
  (define (oui x y z) (make-rational (+ (* (car x) (car y)) (car z))
				   (* (cdr x) (cdr y) (cdr z))))
  (put 'oui '(rational rational rational) oui)
  'done)

(install-random-rational-package)
