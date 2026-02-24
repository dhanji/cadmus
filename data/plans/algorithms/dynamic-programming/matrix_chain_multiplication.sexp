;; Matrix chain multiplication: minimum scalar multiplications
;; Example: dims (10 30 5 60) → 3 matrices, min cost = 4500
;; expected: 4500

(define (matrix_chain_multiplication (n : Number))
  (bind n 4)
  (matrix_chain_multiplication)
)
