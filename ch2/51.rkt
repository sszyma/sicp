#lang sicp

(#%require sicp-pict)

(define (below1 p1 p2)
  (let ((top (transform-painter p1
                                 (make-vect 0 0.5)
                                 (make-vect 1 0.5)
                                 (make-vect 0 1)))
        (bottom (transform-painter p2
                                  (make-vect 0 0)
                                  (make-vect 1 0)
                                  (make-vect 0 0.5))))
    (lambda (frame)
      (top frame)
      (bottom frame))))

(define (below2 p1 p2)
  (define (beside p1 p2)
    (let ((left (transform-painter p1
                                   (make-vect 0 0)
                                   (make-vect 0.5 0)
                                   (make-vect 0 1)))
          (right (transform-painter p2
                                    (make-vect 0.5 0)
                                    (make-vect 1 0)
                                    (make-vect 0.5 1))))
      (lambda (frame)
        (left frame)
        (right frame))))
  (define (rotate180 painter)
    (transform-painter painter
                       (make-vect 1 1)
                       (make-vect 0 1)
                       (make-vect 1 0)))
  (define (rotate270 painter)
    (transform-painter painter
                       (make-vect 1 0)
                       (make-vect 1 1)
                       (make-vect 0 0)))

  (rotate180
   (rotate270
    (beside (rotate270 p1) (rotate270 p2)))))
