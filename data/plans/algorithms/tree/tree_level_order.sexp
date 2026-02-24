;; Binary tree level-order (BFS) traversal
;; Tree: (1 (2 (4 () ()) (5 () ())) (3 () (6 () ())))
;; Levels: ((1) (2 3) (4 5 6))
;; expected: ((1) (2 3) (4 5 6))

(define (tree_level_order (tree : (List Any)))
  (bind tree (list 1 (list 2 (list 4 (list) (list)) (list 5 (list) (list))) (list 3 (list) (list 6 (list) (list)))))
  (tree_level_order)
)
