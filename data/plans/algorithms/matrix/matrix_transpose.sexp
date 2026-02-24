;; Matrix transpose: swap rows and columns
;; Example: ((1 2 3) (4 5 6)) → ((1 4) (2 5) (3 6))
;; expected: ((1 4) (2 5) (3 6))

(define (matrix_transpose (mat : (List (List Number))))
  (bind mat (list (list 1 2 3) (list 4 5 6)))
  (matrix_transpose)
)
