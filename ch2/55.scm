;; how it is evaluated
(car ''abracadabra)
(car (quote (quote abracadabra)))
(car '(quote abracadabra))
'quote
