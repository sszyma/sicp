;; pick 7 from each of the following:

(display (car (cdr (car (cdr (cdr '(1 3 (5 7) 9)))))))

(display (caar '((7))))

;; I completely forgot that this is a list of lists
(display (cadr (cadr (cadr (cadr (cadr (cadr '(1 (2 (3 (4 (5 (6 7)))))))))))))
