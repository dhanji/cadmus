;; Tree mirror: invert a binary tree
;; Tree: (1 (2 () ()) (3 () ()))
;; Mirror: (1 (3 () ()) (2 () ()))
;; expected: (1 (3 () ()) (2 () ()))

(define (tree_mirror (tree : (List Any)))
  (bind tree (list 1 (list 2 (list) (list)) (list 3 (list) (list))))
  (tree_mirror)
)
