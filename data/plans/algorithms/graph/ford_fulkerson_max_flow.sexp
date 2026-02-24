;; Ford-Fulkerson max flow using BFS (Edmonds-Karp)
;; Graph: 0→1(10) 0→2(10) 1→2(2) 1→3(4) 1→4(8) 2→4(9) 3→5(10) 4→3(6) 4→5(10)
;; Max flow from 0 to 5 = 19
;; expected: 19.0

(define (ford_fulkerson_max_flow (n : Number))
  (bind n 6)
  (ford_fulkerson_max_flow)
)
