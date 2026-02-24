;; A star search: shortest path with heuristic
;; Graph: 0→1(1) 0→2(4) 1→3(2) 1→2(2) 2→3(1) 3→4(3)
;; Path from 0 to 4: (0 1 3 4)
;; expected: (0 1 3 4)

(define (a_star_search (n : Number))
  (bind n 5)
  (a_star_search)
)
