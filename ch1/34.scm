;; Suppose we define the procedure
(define (f g) (g 2))
;; What happens if we (perversely) ask the interpreter
;; to evaluate the combination (f f)? Explain.

;; Using substitution model for procedure application, we have
;; (f f)
;; (f 2)
;; (2 2)
;; where the last expression uses 2 as an operator which is wrong
