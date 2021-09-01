;; Alyssa's implementation of the 'if' special form,
;; the sqrt-iter procedure is rewritten using it.
;; The problem is that the interpreter using
;; applicative-order evaluation will evaluate the
;; parameters first, including the procedure call to
;; sqrt-iter itself. Therefore, the interpeter will
;; never return any value.
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))


(define (sqrt x)
  (sqrt-iter GUESS x))

(define GUESS 1.0)

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (define (square x) (* x x))
  (< (abs (- (square guess) x)) TOLERANCE))

(define TOLERANCE 0.001)
