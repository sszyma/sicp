(load "78.scm")

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
  ;; interface for the rest of the system
  (put 'equ? '(scheme-number scheme-number) =)
  (put 'equ? '(rational rational) =rat)
  (put 'equ? '(complex complex) =complex)
  (put '=zero? '(scheme-number) zero?-schemenum)
  (put '=zero? '(rational) zero?-rat)
  (put '=zero? '(complex) zero?-complex)
  'done)

(install-general-arith-package)

(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
