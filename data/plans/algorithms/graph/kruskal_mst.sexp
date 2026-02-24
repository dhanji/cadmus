;; Kruskal's MST: minimum spanning tree using union-find
;; Edges: (0,1,4) (0,2,1) (1,2,2) (1,3,5) (2,3,3)
;; MST weight = 1+2+3 = 6
;; expected: 6

(define (kruskal_mst (n : Number))
  (bind n 4)
  (kruskal_mst)
)
