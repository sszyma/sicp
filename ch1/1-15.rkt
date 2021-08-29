#lang sicp

(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;; Let n be the recursion level of the procedure application (sine 12.5), then
;; we have that 12.5 * 3^(-n) <= 0.1, so  n >= log_3_(12.5/0.1) = log125/log3.
;; Therefore the space complexity of the sine procedure is ceiling{loga/log3} = O(logx).
;; Because with each recursive call two new recursive calls are generated, then the time
;; complexity of sine is O(2^(logx) = O(e^(log2 * logx)) = O(x^log2).   
