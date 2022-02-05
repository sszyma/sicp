(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))


(define (make-table)
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

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (error
	   "No method for these types: APPLY-GENERIC"
	   (list op type-tags))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (deriv exp var)
  (define (operator exp) (car exp))
  (define (operands exp) (cdr exp))
  (cond ((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	(else ((get 'deriv (operator exp))
	       (operands exp) var))))

;; (a) We cannot assimilate the predicates number? and variable? into the lookup table
;; since any predicate, if properly constructed, is supposed to work with any 'type', even
;; when unspecified. Also the idea was to treat the arithmetic operations as the possible
;; types for a procedure to be differentiated.

;; (b) /(c) Suppose that there can be only two operands
(define (install-deriv-package)
  (define (sum operands var)
    (attach-tag '+
		(list (deriv (car operands) var)
		      (deriv (cadr operands) var))))
  (define (product operands var)
    (attach-tag '+
		(attach-tag '* (list (deriv (car operands) var)
				     (cadr operands)))
		(attach-tag '* (list (car operands)
				     (deriv (cadr operands) var)))))
  ;; ugly as hell but who cares
  (define (quotient operands var)
    (attach-tag '/
		(attach-tag '+
			    (list (deriv (car operands) var)
				     (cadr operands))
			    (attach-tag '* (list -1
						 (car operands)
						 (deriv (cadr operands) var))))
		(attach-tag '* (list (cadr operands) (cadr operands)))))
  ;; interface for the rest of the system
  (put 'deriv '(+) sum)
  (put 'deriv '(*) product)
  (put 'deriv '(/) quotient)
  'done)

;; (d) If the dispatch line of the deriv procedure looked like this:

;; ((get (operator exp) 'deriv) (operands exp) var),

;; then the operation to be performed on the object would be the type,
;; in this case differentiation, and all the arithmetic operation would
;; be invoked under their respective names, which requires us only to
;; swap the symbols in the interface section of the differentiation package:

;;  (put '+ '(deriv) sum)
;;  (put '* '(deriv) product)
;;  (put '/ '(deriv) quotient)
;;  'done)
