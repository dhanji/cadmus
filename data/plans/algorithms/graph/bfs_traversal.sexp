;; BFS traversal: breadth-first search from start node
;; Graph: 0→1,2  1→3  2→3,4  3→5  4→5
;; Example: BFS from 0 = (0 1 2 3 4 5)
;; expected: (0 1 2 3 4 5)

(define (bfs_traversal (n : Number))
  (bind n 6)
  (bfs_traversal)
)
