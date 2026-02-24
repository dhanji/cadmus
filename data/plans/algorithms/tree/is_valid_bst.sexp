;; Check if binary tree is a valid BST
;; Tree: (2 (1 () ()) (3 () ())) — valid BST
;; expected: #t

(define (is_valid_bst (tree : (List Any)))
  (bind tree (list 2 (list 1 (list) (list)) (list 3 (list) (list))))
  (is_valid_bst)
)
