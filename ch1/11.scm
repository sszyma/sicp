;; A direct, recursive implementation of the function f
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

;; An iterative implementation
(define (f2 n)
  (define (iter a b c count)
    (if (= count 0)
        c
        (iter (+ a (* 2 b) (* 3 c))
              a
              b
              (- count 1))))
  (iter 2 1 0 n))
