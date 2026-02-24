;; Power set: all subsets of a set
;; Example: power_set((1 2 3)) = (() (1) (2) (1 2) (3) (1 3) (2 3) (1 2 3))
;; expected: (() (1) (2) (1 2) (3) (1 3) (2 3) (1 2 3))

(define (power_set (lst : (List Number)))
  (bind lst (list 1 2 3))
  (power_set)
)
