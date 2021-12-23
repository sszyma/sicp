(load "67.scm")

;; This is a very inefficient procedure that does not
;; make use of the data structure. It basically scans
;; the whole tree.
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

;; A bit better I suppose
(define (encode2 message tree)
  (define (encode-symbol symbol branch)
    (cond ((and (leaf? branch) (eq? symbol (symbol-leaf branch)))
	   '())
	  ((memq symbol (symbols (left-branch branch)))
	   (cons 0 (encode-symbol symbol (left-branch branch))))
	  ((memq symbol (symbols (right-branch branch)))
	   (cons 1 (encode-symbol symbol (right-branch branch))))
	  (else
	   (error "NO SYMBOL IN TREE" "SEARCH-SYMBOL-PATH" symbol tree))))
  (fold-right (lambda (sym rst)
		(append (encode-symbol sym tree) rst))
	      '()
	      message))
