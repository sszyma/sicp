(load "67.scm")

(define (encode message tree)
  (define (encode-symbol symbol tree)
    ;; Tree [List-of Bits] -> [List-of Bits]
    (define (search-symbol-path draft-branch bits)
      (if (leaf? draft-branch)
	  (if (eq? symbol (symbol-leaf draft-branch))
	      bits
	      #f)
	  (let ((potential-left
		 (search-symbol-path (left-branch draft-branch)
				     bits))
		(potential-right
		 (search-symbol-path (right-branch draft-branch)
				     bits)))
	    (cond ((list? potential-left)
		   (cons 0 potential-left))
		  ((list? potential-right)
		   (cons 1 potential-right))
		  (else #f)))))
    (let ((result (search-symbol-path tree '())))
      (if result
	  result
	  (error "NO SYMBOL IN TREE"
		 "SEARCH-SYMBOL-PATH"
		 symbol
		 tree))))
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))
