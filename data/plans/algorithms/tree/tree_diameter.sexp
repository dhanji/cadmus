;; Binary tree diameter: longest path between any two nodes
;; Tree: (1 (2 (4 () ()) (5 () ())) (3 () ()))
;; Diameter: 3 (path 4→2→1→3 or 5→2→1→3)
;; expected: 3

(define (tree_diameter (tree : (List Any)))
  (bind tree (list 1 (list 2 (list 4 (list) (list)) (list 5 (list) (list))) (list 3 (list) (list))))
  (tree_diameter)
)
