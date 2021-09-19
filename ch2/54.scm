;; do the two lists have the same symbols in the same order
(define (equal? los1 los2)
  (if (and (null? los1) (null? los2))
      #t
      (if (and (pair? los1) (pair? los2))
	  (let ((first1 (car los1))
		(frist2 (car los2)))
	    (and (symbol? first1) (symbol? first2)
		 (eq? first1 first2)
		 (equal? (cdr los1) (cdr los2))))
	  #f)))


