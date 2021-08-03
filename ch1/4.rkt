#lang sicp

;; Number Number -> Number
;; add to a the absolute value of b
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
