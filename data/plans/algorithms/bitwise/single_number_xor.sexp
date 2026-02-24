;; Single number: find element appearing once (others appear twice)
;; Example: single in (2 3 5 3 2) = 5
;; expected: 5

(define (single_number_xor (lst : (List Number)))
  (bind lst (list 2 3 5 3 2))
  (single_number_xor)
)
