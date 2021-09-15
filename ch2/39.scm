(define (reverse1 seq)
  (fold-right (lambda (x y) (append y (list x)))
	      '()
	      seq))

(define (reverse2 seq)
  (fold-left (lambda (x y) (cons y x)) '() seq))
