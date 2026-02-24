;; Generate permutations: all orderings of n elements
;; Example: permutations of (1 2 3) = ((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))
;; expected: ((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))

(define (generate_permutations (n : Number))
  (bind n 3)
  (generate_permutations)
)
