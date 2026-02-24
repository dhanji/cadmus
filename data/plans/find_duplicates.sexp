;; Find duplicates: walk, filter all, sort
(define (find-duplicates (path : Dir))
  (walk_tree)
  (filter :extension "*")
  (sort_by "name"))
