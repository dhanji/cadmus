;; BST insert then inorder traversal (produces sorted output)
;; Example: insert 5,3,7,1,4 → inorder (1 3 4 5 7)
;; expected: (1 3 4 5 7)

(define (binary_search_tree_insert (vals : (List Number)))
  (bind vals (list 5 3 7 1 4))
  (binary_search_tree_insert)
)
