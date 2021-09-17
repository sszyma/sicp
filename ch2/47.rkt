#lang sicp

(#%require sicp-pict)


(define (make-frame.v1 origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin.v1 f) (car f))
(define (edge1.v1 f) (cadr f))
(define (edge2.v1 f) (caddr f))

(define (make-frame.v2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (origin.v2 f) (car f))
(define (edge1.v2 f) (cadr f))
(define (edge2.v2 f) (cddr f))
