;; Prim's MST: minimum spanning tree using greedy approach
;; Edges: (0,1,4) (0,2,1) (1,2,2) (1,3,5) (2,3,3)
;; MST weight = 1+2+3 = 6
;; expected: 6

(define (prim_mst (n : Number) (adj : Number))
  (bind n 4)
  (bind adj (vector (list (cons 1 4) (cons 2 1)) (list (cons 0 4) (cons 2 2) (cons 3 5)) (list (cons 0 1) (cons 1 2) (cons 3 3)) (list (cons 1 5) (cons 2 3))))
  (prim_mst)
)
