;; Insertion sort
;; Example: sort (5 3 8 1 9 2 7 4 6) = (1 2 3 4 5 6 7 8 9)
;; expected: (1 2 3 4 5 6 7 8 9)

(define (insertion_sort (lst : (List Number)))
  (bind lst (list 5 3 8 1 9 2 7 4 6))
  (insertion_sort)
)
