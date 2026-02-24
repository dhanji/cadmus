;; Binary tree preorder traversal
;; Tree: (1 (2 (4 () ()) (5 () ())) (3 () (6 () ())))
;; Preorder: (1 2 4 5 3 6)
;; expected: (1 2 4 5 3 6)

(define (tree_preorder (tree : (List Any)))
  (bind tree (list 1 (list 2 (list 4 (list) (list)) (list 5 (list) (list))) (list 3 (list) (list 6 (list) (list)))))
  (tree_preorder)
)
