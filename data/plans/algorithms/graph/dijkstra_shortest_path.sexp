;; Dijkstra's shortest path: find shortest distances from source
;; Graph: 0→1(4) 0→2(1) 1→3(1) 2→1(2) 2→3(5) 3→4(3)
;; Example: distances from 0 = (0 3 1 4 7)
;; expected: (0 3 1 4 7)

(define (dijkstra_shortest_path (n : Number))
  (bind n 5)
  (dijkstra_shortest_path)
)
