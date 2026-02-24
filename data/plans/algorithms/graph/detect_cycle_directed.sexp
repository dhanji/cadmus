;; Detect cycle in directed graph using DFS coloring
;; Graph: 0→1→2→3→1 (cycle 1→2→3→1)
;; expected: #t

(define (detect_cycle_directed (n : Number))
  (bind n 4)
  (detect_cycle_directed)
)
