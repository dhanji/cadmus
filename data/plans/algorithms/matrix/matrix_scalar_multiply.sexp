;; Matrix scalar multiplication: k × M
;; Example: 3 × ((1 2) (3 4)) = ((3 6) (9 12))
;; expected: ((3 6) (9 12))

(define (matrix_scalar_multiply (mat : (List (List Number))) (k : Number))
  (bind k 3)
  (bind mat (list (list 1 2) (list 3 4)))
  (matrix_scalar_multiply)
)
