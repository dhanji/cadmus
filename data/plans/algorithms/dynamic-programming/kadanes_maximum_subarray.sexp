;; Kadane's algorithm: maximum subarray sum
;; Example: max subarray of (-2 1 -3 4 -1 2 1 -5 4) = 6
;; expected: 6

(define (kadanes_maximum_subarray (lst : (List Number)))
  (bind lst (list -2 1 -3 4 -1 2 1 -5 4))
  (kadanes_maximum_subarray)
)
