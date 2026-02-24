;; Union-Find: count connected components after unions
;; 5 nodes, union(0,1) union(2,3) union(0,2) → 2 components
;; expected: 2

(define (union_find (n : Number))
  (bind n 5)
  (union_find)
)
