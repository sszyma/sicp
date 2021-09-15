(define (square-list1 items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (cons ((lambda (x) (* x x)) (car things))
		    answer))))
  (iter items '()))
;; This procedure won't work as desired, because at each
;; iteration the first element on the 'things' list
;; is put on top of the 'answer' list, which contains as
;; the first element the preceeding element from the original
;; list.

(define (square-list2 items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (cons answer
		    ((lambda (x) (* x x)) (car things))))))
  (iter items '()))
;; This won't work either because the accumulator answer produces nested
;; pairs not forming any list
