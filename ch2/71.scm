;; Suppose that we make a tree for an alphabet of n letters of
;; frequences 1, 2, 4, ..., 2^(n-1). Since
;; 2^(k-1) + 2^k = 2^k(1 + 1/2) = 3/2*2^k < 2^(k+1),
;; for all natural k, then the corresponding Huffman tree will
;; have one main branch (obviously) and (n-1) leaves (not branches).
;; Thus the first symbol will require one bit, the second one will
;; require one bit, the next four symbols will require two bits,
;; the next 8 symbols 3 bits and so on. To find the encoding of the
;; last symbol we want to find the smallest positive integer k such that
;;                      n < 2^0 + 2^1 + ... + 2^k
;;                        = (1-2^(k+1))/(1-2)
;;                        = 2^(k+1) - 1,
;; which implies that the last symbol requires
;;                     k=ceiling(log_2(n+1)-1)
;; bits.
