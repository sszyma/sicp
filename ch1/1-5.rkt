#lang sicp

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;; The expression
;; (test 0 (p))
;; contains an operand (p), where p is a function that returns itself
;; indefinitely. Therefore, inn applicative-order evaluation the
;; interpreter will first attempt to evaluate all the operands, including
;; the procedure p which will not return any value, and so will do the interpreted
;; expression. In normal-order evaluation, the operands  will be
;; evaluated when needed, and since the first case of the conditional
;; in the test procedure is satisfied then the interpreter will return 0,
;; disregarding the operand p.
