;; Matrix exponentiation: M^k using repeated squaring
;; Example: ((1 1) (1 0))^6 = ((13 8) (8 5))
;; expected: ((13 8) (8 5))

(define (matrix_power (mat : (List (List Number))) (k : Number))
  (bind k 6)
  (bind mat (list (list 1 1) (list 1 0)))
  (matrix_power)
)
