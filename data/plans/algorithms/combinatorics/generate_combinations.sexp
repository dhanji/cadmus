;; Generate combinations: all k-combinations of n elements
;; Example: 2-combinations of (1..4) = ((1 2) (1 3) (1 4) (2 3) (2 4) (3 4))
;; expected: ((1 2) (1 3) (1 4) (2 3) (2 4) (3 4))

(define (generate_combinations (n : Number) (k : Number))
  (bind k 2)
  (bind n 4)
  (generate_combinations)
)
