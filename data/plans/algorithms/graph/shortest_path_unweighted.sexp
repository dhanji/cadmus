;; Shortest path unweighted: BFS-based shortest path
;; Graph: 0-1, 0-2, 1-3, 2-3, 2-4, 3-5, 4-5
;; Distances from 0: (0 1 1 2 2 3)
;; expected: (0 1 1 2 2 3)

(define (shortest_path_unweighted (n : Number))
  (bind n 6)
  (shortest_path_unweighted)
)
