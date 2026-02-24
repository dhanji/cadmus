;; Identity matrix of size n×n
;; Example: I(3) = ((1 0 0) (0 1 0) (0 0 1))
;; expected: ((1 0 0) (0 1 0) (0 0 1))

(define (matrix_identity (n : Number))
  (bind n 3)
  (matrix_identity)
)
