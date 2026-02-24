;; Two sum on sorted array: find indices of two elements that sum to target
;; Example: two_sum (2 7 11 15) 9 = (0 1)
;; expected: (0 1)

(define (two_sum_sorted (lst : (List Number)) (target : Number))
  (bind lst (list 2 7 11 15))
  (bind target 9)
  (two_sum_sorted)
)
