;; reverse every list and sublist
(define (deep-reverse list)
  (map (lambda (i) (if (list? i)
		       (deep-reverse i)
		       i))
       (reverse list)))
