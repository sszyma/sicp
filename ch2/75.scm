;; Generic complex number constructor in the message passing style
(define (make-from-mag-ang r x)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos x)))
	  ((eq? op 'imag-part) (* r (sin x)))
	  ((eq? op 'magnitude) r)
	  ((eq? op angle) x)
	  (else (error "Unknown op" "MAKE-FROM-MAG-ANG" op))))
  dispatch)
