;; Graph coloring greedy: assign minimum colors to vertices
;; Graph: 0-1, 0-2, 1-2, 1-3, 2-3, 3-4
;; expected: (0 1 2 0 1)

(define (graph_coloring_greedy (n : Number))
  (bind n 5)
  (graph_coloring_greedy)
)
