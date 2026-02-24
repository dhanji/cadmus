;; Counting sort: sort non-negative integers by counting occurrences
;; Example: sort (4 2 2 8 3 3 1) = (1 2 2 3 3 4 8)
;; expected: (1 2 2 3 3 4 8)

(define (counting_sort (lst : (List Number)))
  (bind lst (list 4 2 2 8 3 3 1))
  (counting_sort)
)
