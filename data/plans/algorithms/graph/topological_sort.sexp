;; Topological sort using Kahn's algorithm (BFS-based)
;; DAG: 0→1,2  1→3  2→3,4  4→5  3→5
;; Example: one valid order = (0 1 2 3 4 5)
;; expected: (0 1 2 3 4 5)

(define (topological_sort (n : Number))
  (bind n 6)
  (topological_sort)
)
