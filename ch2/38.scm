(define (fold-left op init seq)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter init seq))

(fold-right / 1 '(1 2 3)) ;; -> 1/(2/(3/1)) = 3/2

(fold-left / 1 '(1 2 3)) ;; -> ((1/1)/2)/3) = 1/6

(fold-right list '() '(1 2 3)) ;; -> (list 1 (list 2 (list 3 '()))) == '(1 (2 (3 ())))

(fold-left list '() '(1 2 3)) ;; -> (list (list (list '() 1) 2) 3) == '(((() 1) 2) 3)

;; to guarantee that fold-right and fold-left will produce the same values for
;; any sequence, op should be commutative
