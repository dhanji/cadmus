;; Check if binary tree is height-balanced
;; Tree: (1 (2 (3 () ()) ()) ()) — left-heavy, not balanced
;; expected: #f

(define (is_balanced_tree (tree : (List Any)))
  (bind tree (list 1 (list 2 (list 3 (list) (list)) (list)) (list)))
  (is_balanced_tree)
)
