;; Matrix determinant using cofactor expansion
;; Example: det((1 2 3) (4 5 6) (7 8 0)) = 27
;; expected: 27

(define (matrix_determinant (mat : (List (List Number))))
  (bind mat (list (list 1 2 3) (list 4 5 6) (list 7 8 0)))
  (matrix_determinant)
)
