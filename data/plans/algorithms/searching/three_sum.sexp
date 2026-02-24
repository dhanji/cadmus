;; Three sum: find all triplets that sum to zero
;; Example: (-1 0 1 2 -1 -4) → ((-1 -1 2) (-1 0 1))
;; expected: ((-1 -1 2) (-1 0 1))

(define (three_sum (nums : (List Number)))
  (bind nums (list -1 0 1 2 -1 -4))
  (three_sum)
)
