;; Delete DS_Store: recursively delete all .DS_Store files from the current directory
(define (delete-ds-store (path : Dir))
  (walk_tree)
  (find_matching :pattern ".DS_Store")
  (delete :each))