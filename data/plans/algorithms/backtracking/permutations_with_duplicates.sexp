;; Unique permutations of a list that may contain duplicates
;; Example: (1 1 2) → ((1 1 2) (1 2 1) (2 1 1))
;; expected: ((1 1 2) (1 2 1) (2 1 1))

(define (permutations_with_duplicates (nums : (List Number)))
  (bind nums (list 1 1 2))
  (permutations_with_duplicates)
)
