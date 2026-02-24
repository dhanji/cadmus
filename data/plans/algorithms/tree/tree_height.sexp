;; Tree height: compute maximum depth of a binary tree
;; Tree: (1 (2 (4 () ()) (5 () ())) (3 () (6 () ())))
;; Height: 3
;; expected: 3

(define (tree_height (tree : (List Any)))
  (bind tree (list 1 (list 2 (list 4 (list) (list)) (list 5 (list) (list))) (list 3 (list) (list 6 (list) (list)))))
  (tree_height)
)
