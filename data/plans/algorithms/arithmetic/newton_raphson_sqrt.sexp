;; Newton-Raphson square root approximation
;; Example: sqrt(2) ≈ 1.4142135623730951
;; expected: 1.414213562373095

(define (newton_raphson_sqrt (n : Number))
  (bind n 2)
  (newton_raphson_sqrt)
)
