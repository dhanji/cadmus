;; Fisher Yates shuffle: random permutation of a list
;; Example: shuffle (1 2 3 4 5) with seed 42
;; expected: (2 4 3 5 1)

(define (fisher_yates_shuffle (lst : (List Number)))
  (bind lst (list 1 2 3 4 5))
  (fisher_yates_shuffle)
)
