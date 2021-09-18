#lang sicp

(#%require sicp-pict)

;; a) 
(define antiwave
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
         (make-segment (make-vect 1 9/16) (make-vect 1 11/16))
         (make-segment (make-vect 0 0) (make-vect 1 1))
         (make-segment (make-vect 0 1) (make-vect 1 0)))))

;; b)
(define (up-split p n)
    (if (= n 0)
        p
        (let ((next (up-split p (- n 1))))
          (below p (beside next next)))))

(define (right-split p n)
  (if (= n 0)
      p
      (let ((next (right-split p (- n 1))))
        (beside p (below next next)))))


(define (corner-split p n)
  (if (= n 0)
      p
      (beside (below p (up-split p (- n 1)))
              (below (right-split p (- n 1))
                     (corner-split p (- n 1))))))

;; c)
(define (square-limit p n)
  (define (square-of-four tl tr bl br)
    (below (beside bl br) (beside tl tr)))
  (let ((corner (corner-split p (- n 1))))
    (square-of-four corner
                    (flip-horiz corner)
                    (flip-vert corner)
                    (flip-vert (flip-horiz corner)))))
