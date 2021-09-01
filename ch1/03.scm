;; Number Number Number -> Number
;; takes three numbers and sums the square of the two larger ones
(define (sum-sqr-larger a b c)
  ;; Number -> Number
  (define (square x) (* x x))
  ;; Number Number -> Number
  ;; sums the squares of two numbers
  (define (sum-sqr x y)
    (+ (square x) (square y)))
  (if (> a b)
      (sum-sqr a (if (> b c) b c))
      (sum-sqr b (if (> a c) a c))))
