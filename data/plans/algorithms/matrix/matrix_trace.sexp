;; Matrix trace: sum of diagonal elements
;; Example: trace((1 2 3) (4 5 6) (7 8 9)) = 1+5+9 = 15
;; expected: 15

(define (matrix_trace (mat : (List (List Number))))
  (bind mat (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
  (matrix_trace)
)
