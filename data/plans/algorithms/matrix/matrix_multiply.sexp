;; Matrix multiplication: A × B
;; Example: ((1 2) (3 4)) × ((5 6) (7 8)) = ((19 22) (43 50))
;; expected: ((19 22) (43 50))

(define (matrix_multiply (a : (List (List Number))) (b : (List (List Number))))
  (bind a (list (list 1 2) (list 3 4)))
  (bind b (list (list 5 6) (list 7 8)))
  (matrix_multiply)
)
