;;;;;;;;;;;;;;;;;;;;

(define (square x) (* x x))

(define (sqrt0 x)
  (sqrt-iter good-enough0? GUESS x))

(define GUESS 1.0)

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sqrt-iter accuracy-test guess x)
  (if (accuracy-test guess x)
      guess
      (sqrt-iter accuracy-test
                 (improve guess x)
                 x)))

(define (good-enough0? guess x)
  (< (abs (- (square guess) x)) TOLERANCE))

(define TOLERANCE 0.001)

;; Below we verify whether the good-enough0? procedure
;; is ineffective for finding the square roots of very large and
;; very small numbers:

(square (sqrt0 0.00000025))

(square (sqrt0 0.000081))

(square (sqrt0 123123123123123123))

(square (sqrt0 999999999999999999))


;; At least in racket, in case of very small
;; numbers there was a significant difference between
;; computed and the actual value of the square root.
;; In case of very large (18-digit) numbers there was
;; a difference of one order of magnitude.
;; I have no idea why there is such a discrepancy in
;; accuracy of square roots of very large and very small
;; numbers, but anyway both of them are inaccurate due to
;; many iterations of arithmetic operations on real numbers
;; which have limited precision.

;; A suggested corrected version of good-enough0?, which
;; observes the rate of change of successive guesses of the
;; square root of a number.
(define (good-enough1? guess x)
  (< (abs (/ (- guess (improve guess x)) guess)) TOLERANCE))

(define (sqrt1 x)
  (sqrt-iter good-enough1? GUESS x))

(square (sqrt1 0.00000025))

(square (sqrt1 0.000081))

(square (sqrt1 123123123123123123))

(square (sqrt1 999999999999999999))

;; Now the square root computations are significantly more
;; accurate in case of very small numbers, but a bit less
;; accurate in case of very large numbers.



