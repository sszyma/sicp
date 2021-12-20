(load "69.scm")

(define pairs '((A 2)
		(BOOM 1)
		(GET 2)
		(JOB 2)
		(NA 16)
		(SHA 3)
		(YIP 9)
		(WAH 1)))

(define tree (generate-huffman-tree pairs))

(define msg '(GET A JOB
		  SHA NA NA NA NA NA NA NA NA
		  GET A JOB
		  SHA NA NA NA NA NA NA NA NA
		  WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
		  SHA BOOM))

(define bits (encode msg tree))

(display bits)
(newline)
(display (decode bits tree))

;; 84 bits were needed to encode the message using Huffman trees.
;; If we were to use a fixed-length code the same message would
;; require log_2(8) * 36 = 118 bits to encode. 
