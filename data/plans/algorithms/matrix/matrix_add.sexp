;; Matrix addition: A + B
;; Example: ((1 2) (3 4)) + ((5 6) (7 8)) = ((6 8) (10 12))
;; expected: ((6 8) (10 12))

(define (matrix_add (a : (List (List Number))) (b : (List (List Number))))
  (bind a (list (list 1 2) (list 3 4)))
  (bind b (list (list 5 6) (list 7 8)))
  (matrix_add)
)
