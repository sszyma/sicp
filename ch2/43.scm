;; Board is a [List-of Position], that is n x n array
;; Postion is (cons Number Number)

;; outputs a sequence of all solutions to a problem of placing
;; n queens on an n x n chessboard 
(define (queens board-size) ;; board-size = n
  (define (empty-board) '())
  (define (enumerate-interval l u)
    (if (> l u)
	'()
	(cons l (enumerate-interval (+ l 1) u))))
  (define (adjoin-position new-row k rest-of-queens)
    (cons (cons new-row k) rest-of-queens))
  (define (safe? k positions)
    (let ((kth (car (filter (lambda (pos)
			      (= (cdr pos) k))
			    positions))))
      (let ((rest (remove kth positions))
	    (kx (car kth))
	    (ky (cdr kth)))
	(fold-right (lambda (x y) (and x y))
		    #t (map (lambda (pos)
			      (let ((x (car pos))
				    (y (cdr pos)))
				(not (or (= x kx)
					 (= y ky)
					 (= (+ x y) (+ kx ky))
					 (= (- x y) (- kx ky))))))
			    rest)))))
  (define (queen-cols k)
    (if (= k 0)
	(list (empty-board))
	(filter (lambda (positions) (safe? k positions))
		(fold-right append '()
			    (map (lambda (new-row);;;;;;;;;;;;;;;; this has changed w.r.t. ex. 42
				   (map (lambda (rest-of-queens)
					  (adjoin-position new-row k rest-of-queens))
					(queen-cols (- k 1))))
				 (enumerate-interval 1 board-size))))));;;;;;;;;;;;;;;;;;;;;;;
  (queen-cols board-size))

;; this nested mapping computes (queen-cols (- k 1)) k times for each k,
;; so if T(k) = O(k) was the time necessary to solve the puzzle for k in ex. 42,
;; then here the time necessary to solve the same puzzle is O(k^k) = k^(k-1)T(k),
;; since at each iteration k new procedure calls are produced.
