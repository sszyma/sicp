;; returns a list with the last element of a nonempty list
(define (last-pair list)
  (let ((next (cdr list)))
    (if (null? next)
	list
	(last-pair next))))
