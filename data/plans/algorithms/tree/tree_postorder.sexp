;; Binary tree postorder traversal
;; Tree: (1 (2 (4 () ()) (5 () ())) (3 () (6 () ())))
;; Postorder: (4 5 2 6 3 1)
;; expected: (4 5 2 6 3 1)

(define (tree_postorder (tree : (List Any)))
  (bind tree (list 1 (list 2 (list 4 (list) (list)) (list 5 (list) (list))) (list 3 (list) (list 6 (list) (list)))))
  (tree_postorder)
)
