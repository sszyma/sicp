(define (install-raise-package)
  (define (integer->rational n) (make-rational n 1))
  (define (rational->schemenum r) (exact->inexact r))
  (define (real->complex x) (make-from-real-imag x 0))
  (put-coercion 'integer 'rational integer->rational)
  (put-coercion 'rational 'scheme-number rational->schemenum)
  (put-coercion 'scheme-number 'complex real->complex)
  'done)

;; To raise an object it suffices to maintain a raise package that
;; contains the procedures that coerce a type to the next one in the
;; tower and a TOWER itself which is an ordered list specifying
;; what a given type can be raised to. 
(define (raise x)
  (let ((tag (type-tag x)))
    (let ((target (memq tag TOWER)))
      (if target
	  (if (null? (cdr target))
	      (error "Cannot raise this type" "RAISE" (list tag TOWER))
	      ((get-coercion tag (cadr target)) (contents x)))
	  (error "Cannot raise this type" "RAISE" (list tag TOWER))))))

(define TOWER '(integer rational real complex))
