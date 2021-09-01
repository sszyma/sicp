;; Expressions with their respective values in the comments

10 ; => 10

(+ 5 3 4) ; => 12

(- 9 1) ; => 8

(+ (* 2 4) (- 4 6)) ; => 6

(define a 3) ; => no output

(define b (+ a 1)) ; => no output

(+ a b (* a b)) ; => 19

(= a b) ; => #f

(if (and (> b a) (< b (* a b)))
    b ; => 4
    a) 

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a)) ; => 16
      (else 25))

(+ 2 (if (> b a) b a)) ; => 6

(* (cond ((> a b) a)
	 ((< a b) b)
	 (else -1))
   (+ a 1)) ; => 16
