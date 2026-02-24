;; Median of a list of numbers
;; Example: median_list((3 1 4 1 5 9 2 6)) = 3.5
;; expected: 3.5

(define (median_list (lst : (List Number)))
  (bind lst (list 3 1 4 1 5 9 2 6))
  (median_list)
)
