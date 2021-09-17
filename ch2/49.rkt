#lang sicp

(#%require sicp-pict)

;; sadly, I cannot implement segments->painter by "myself", since the
;; draw-line procedure does not exist in the Racket's sicp-pict language.

;; draws the outline of the designated fram
(define contour
  (segments->painter
   (list (make-segment (make-vect 0 0) (make-vect 0 1))
         (make-segment (make-vect 0 1) (make-vect 1 1))
         (make-segment (make-vect 1 1) (make-vect 1 0))
         (make-segment (make-vect 1 0) (make-vect 0 0)))))

;; draws "X" by connecting the opposite vertices of the frame
(define x
  (segments->painter
   (list (make-segment (make-vect 0 0) (make-vect 1 1))
         (make-segment (make-vect 1 0) (make-vect 0 1)))))

;; draws the diamond shape by connecting the midpoints of the sides of the frame
(define diamond
  (segments->painter
   (list (make-segment (make-vect 0 0.5) (make-vect 0.5 0))
         (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
         (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
         (make-segment (make-vect 0.5 1) (make-vect 0 0.5)))))

;; I have no willpower to create the wave painter, sry
(define wave
  (segments->painter
   (list (make-segment (make-vect 6/16 6/16) (make-vect 10/16 6/16))
         (make-segment (make-vect 6/16 6/16) (make-vect 6/16 10/16)) 
         (make-segment (make-vect 10/16 6/16) (make-vect 10/16 10/16))
         (make-segment (make-vect 0 11/16) (make-vect 2/16 11/16))
         (make-segment (make-vect 2/16 11/16) (make-vect 2/16 9/16))
         (make-segment (make-vect 2/16 9/16) (make-vect 0 9/16))
         (make-segment (make-vect 0 9/16) (make-vect 0 11/16))
         (make-segment (make-vect 1 11/16) (make-vect 14/16 11/16))
         (make-segment (make-vect 14/16 11/16) (make-vect 14/16 9/16))
         (make-segment (make-vect 14/16 9/16) (make-vect 1 9/16))
         (make-segment (make-vect 1 9/16) (make-vect 1 11/16)))))
;; to test evaluate (paint <painter>); sadly there is only one frame to choose
