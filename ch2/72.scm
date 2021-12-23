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

;; Suppose that we consider a tree from exercise 2.71., then it takes
;; (2n+1) steps from memq to find the nth most frequent symbol in the
;; alphabet. If the message we are to encode consists of m symbols, then
;; it will take between m and (2n+1)m steps, on average m(n+1) steps.
;; Since n is a finite constant, then the average order of growth of the
;; encode2 procedure is O(m).
