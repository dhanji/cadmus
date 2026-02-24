;; N-Queens: count solutions for placing n non-attacking queens
;; Example: 8-queens has 92 solutions
;; expected: 92

(define (n_queens (n : Number))
  (bind n 8)
  (n_queens)
)
