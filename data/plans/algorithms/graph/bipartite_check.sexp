;; Bipartite check: determine if graph is 2-colorable using BFS
;; Graph: 0-1, 1-2, 2-3, 3-0 (cycle of 4 = bipartite)
;; expected: #t

(define (bipartite_check (n : Number))
  (bind n 4)
  (bipartite_check)
)
