(load "08.scm")

;; computes half of the distance between the endpoints of the interval x
(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

;; we verify that the sum of the widths of two intervals is
;; equal to the width of the sum of these interval. Let there
;; be I=[a,b] and J=[c,d], a<=b and c<=d, then
;; width(I+J) = width([a+c,b+d])
;;            = ((b + d) - (a + c))/2
;;            = (b-a)/2 + (d-c)/2
;;            = width(I) + width(J).
;; Surprisingly,
;; width(I-J) = width([a-d,b-c])
;;            = ((b-c) - (a-d))/2
;;            = (b-a)/2 + (d-c)/2
;;            = width(I) + width(J).
;; Also,
(define A (make-interval 3 5))
(define B (make-interval 20 6))
(display (= (width (mul-interval A B))
	    (* (width A) (width B))))
(display (= (width (div-interval A B))
	    (/ (width A) (width B))))
;; It is due to the fact that the product or quotient of two
;; uncertainties is a sum of the corresponding relative or
;; percentage uncertainties.
