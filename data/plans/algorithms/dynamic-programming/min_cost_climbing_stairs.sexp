;; Min cost climbing stairs: pay cost[i] to step from i, reach top
;; Example: costs (10 15 20) → min cost 15
;; expected: 15

(define (min_cost_climbing_stairs (cost : (List Number)))
  (bind cost (list 10 15 20))
  (min_cost_climbing_stairs)
)
