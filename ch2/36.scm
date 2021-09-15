;; [X Y -> Z] Z [List-of List] -> [List-of Z]
;; takes as an argument a list of lists of the same number of elements and
;; returns a list of the corresponding elements of each list being accumulated
(define (accumulate-n op init seqs)
  (define (accumulate op init seq)
    (if (null? seq)
	init
	(op (car seq)
	    (accumulate op init (cdr seq)))))
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))
