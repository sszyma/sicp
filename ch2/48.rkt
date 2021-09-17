#lang sicp

(#%require sicp-pict)

;; Segment is a pair of Vectors from the origin to the respective endpoints
(define (make-segment v w) (cons v w))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

