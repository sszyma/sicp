;; Alyssa P. Hacker complains that we went to a lot of
;; extra work in writing expmod. Aer all, she says,
;; since we already know how to compute exponentials, we
;; could have simply written
(define (expmod base exp m)
  (remainder (fast-expt base exp) m))
;; Is she correct? Would this procedure serve as well for our
;; fast prime tester? Explain.

;; The original expmod procedure makes use of multiplication of
;; moduli, thus dealing with smaller numbers, while this version
;; takes a one reminder of a very large number, so it should be less
;; efficient than the original one, due to the fact that, as
;; noted in the text, arithmetic operations on very large numbers
;; consume more resources than "ordinary" numbers.
