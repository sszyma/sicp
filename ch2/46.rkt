#lang sicp

(#%require sicp-pict)


;; Let vector be (cons x y) where x,y are Numbers
(define (make-vector x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

;; Vector Vector -> Vector
(define (add-vect v w)
  (make-vect (+ (xcor-vect v) (xcor-vect w))
             (+ (ycor-vect v) (ycor-vect w))))

;; Vector Vector -> Vector
(define (sub-vect v w)
  (make-vect (- (xcor-vect v) (xcor-vect w))
             (- (ycor-vect v) (ycor-vect w))))

;; Vector Number -> Vector
(define (scale-vect v c)
  (make-vect (* c (xcor-vect v))
             (* c (ycor-vect v))))
