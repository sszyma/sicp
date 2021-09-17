#lang sicp

(#%require sicp-pict)

;; to display painters use (paint <painter>)

(define (up-split p n)
  (if (= n 0)
      p
      (let ((smaller (up-split p (- n 1))))
        (below p (beside smaller smaller)))))


;;;;;;;;;;;;;;;;;;;;;; not related ;;;;;;;;;;;;;;

(define (right-split p n)
  (if (= n 0)
      p
      (let ((smaller (right-split p (- n 1))))
        (beside p (below smaller smaller)))))

(define (corner-split p n)
  (if (= n 0)
      p
      (let ((up-half (up-split p (- n 1)))
            (right-half (right-split p (- n 1))))
        (let ((up (beside up-half up-half))
              (right (below right-half right-half))
              (corner (corner-split p (- n 1))))
          (beside (below p up)
                  (below right corner))))))

(define (square-limit p n)
  (if (= n 0)
      p
      (let ((corner (corner-split p (- n 1))))
        (let ((top-half (beside (flip-horiz corner) corner)))
          (below (flip-vert top-half) top-half)))))
