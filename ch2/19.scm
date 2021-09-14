;; Number [List-of Number] -> Number
;; Given a list of denominations, count on how
;; many ways can you change a given amount
(define (cc amount coin-values)
  (define (no-more? list) (null? list))
  (define (except-first-denomination list) (cdr list))
  (define (first-denomination list) (car list))
  (cond ((= amount 0) 1)
	((or (< amount 0) (no-more? coin-values)) 0)
	(else
	 (+ (cc amount
		(except-first-denomination coin-values))
	    (cc (- amount
		   (first-denomination coin-values))
		coin-values)))))

;; The order of denominations does not matter since cc
;; traverses the whole coin-values list, and hence every
;; item on the list is considered anyway.
