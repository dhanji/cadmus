;; Bellman-Ford: shortest paths with negative edges
;; Graph: 0→1(4) 0→2(1) 1→3(1) 2→1(-2) 2→3(5) 3→4(3)
;; Example: distances from 0 = (0 -1 1 0 3)
;; expected: (0 -1 1 0 3)

(define (bellman_ford (n : Number) (edges : (List (List Number))))
  (bind n 5)
  (bind edges (list (list 0 1 4) (list 0 2 1) (list 1 3 1) (list 2 1 -2) (list 2 3 5) (list 3 4 3)))
  (bellman_ford)
)
