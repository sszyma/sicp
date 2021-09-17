#lang sicp

(#%require sicp-pict)

(define (split og+next next+next)
  (define (helper p n)
    (if (= n 0)
        p
        (let ((next (helper p (- n 1))))
          (og+next p (next+next next next)))))
  helper)

(define right-split (split beside below))
(define up-split (split below beside))
