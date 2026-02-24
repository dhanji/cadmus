;; Arithmetic chain: add(4, 35) -> multiply(_, 2) -> subtract(_, 10)
(define (complex-arith) : Number
  (add :x "4" :y "35")
  (multiply :y "2")
  (subtract :y "10"))
