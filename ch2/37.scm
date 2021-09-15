
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (define (accumulate op init seq)
    (if (null? seq)
	init
	(op (car seq)
	    (accumulate op init (cdr seq)))))
      
;; [X Y -> Z] Z [List-of List] -> [List-of Z]
;; takes as an argument a list of lists of the same number of elements and
;; returns a list of the corresponding elements of each list being accumulated
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Let a vector be a list of numbers, and a matrix be a list of row vectors

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (transpose m)
  (accumulate-n cons '() m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
	   (matrix-*-vector cols row))
	 m)))
  
;; why not?
(define (det m)
  (define (coeff i j)
    (list-ref (list-ref m (- i 1)) (- j 1)))
  (define (remove-list-ref l n)
    (if (null? l)
	'()
	(let ((rest (remove-list-ref (cdr l) (- n 1))))
	  (if (= n 1)
	      rest
	      (cons (car l) rest)))))
  (define (minor i j)
    (map (lambda (row) (remove-list-ref row j))
	 (remove-list-ref m i)))
  (define (enumerate n)
    (if (= n 0)
	'()
	(cons n (enumerate (- n 1)))))
  (let ((square? (= (length (car m)) (length m)))
	(dimension (length m)))
    (if square?
	(if (= dimension 2)
	    (- (* (coeff 1 1) (coeff 2 2))
	       (* (coeff 1 2) (coeff 2 1)))
	    (accumulate + 0
			(map (lambda (k) (* (expt (- 1) (+ 1 k))
					    (coeff 1 k)
					    (det (minor 1 k))))
			     (enumerate dimension))))
	(error "DET" "The input is not a square matrix" m))))

