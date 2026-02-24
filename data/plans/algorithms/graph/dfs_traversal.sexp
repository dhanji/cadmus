;; DFS traversal: depth-first search from start node
;; Graph: 0→1,2  1→3  2→3,4  3→5  4→5
;; Example: DFS from 0 = (0 1 3 5 2 4)
;; expected: (0 1 3 5 2 4)

(define (dfs_traversal (n : Number))
  (bind n 6)
  (dfs_traversal)
)
