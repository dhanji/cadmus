;; Floyd-Warshall: all-pairs shortest paths
;; 4 nodes, edges: 0→1(3) 0→2(6) 1→2(2) 2→3(1) 3→0(7)
;; Example: dist matrix row 0 = (0 3 5 6)
;; expected: (0 3 5 6)

(define (floyd_warshall (n : Number) (d : Number))
  (bind n 4)
  (bind d (vector (vector 0 3 6 +inf.0) (vector +inf.0 0 2 +inf.0) (vector +inf.0 +inf.0 0 1) (vector 7 +inf.0 +inf.0 0)))
  (floyd_warshall)
)
