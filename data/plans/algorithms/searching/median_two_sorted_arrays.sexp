;; Median of two sorted arrays
;; Example: median of (1 3) and (2) = 2
;; expected: 2

(define (median_two_sorted_arrays (a : (List Number)) (b : (List Number)))
  (bind a (list 1 3))
  (bind b (list 2))
  (median_two_sorted_arrays)
)
