;; Number -> Number
;; Computes cube root of a given number using Newton's method
(define (cbrt x)
  (cbrt-iter GUESS x))

(define GUESS 1.0)

(define (improve guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))

(define (square x) (* x x))

(define (cbrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (cbrt-iter (improve guess x)
                 x)))

(define (good-enough? guess x)
  (< (abs (/ (- guess (improve guess x)) guess)) TOLERANCE))

(define TOLERANCE 0.001)


