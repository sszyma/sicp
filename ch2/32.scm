;; Generates a set of subsets of a set,
;; given that a set is represented as a list
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
	(append rest (map (lambda (ss)
			    (cons (car s) ss))
			  rest)))))
;; a list of subsets of a set is formed by combining a list of
;; subsets without the first element, with a list of subsets
;; with the first element
