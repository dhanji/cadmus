;; Deep directory audit: walk, find .rs, filter tests, sort, unique, count
(define (deep-audit (path : Dir))
  (walk_tree)
  (find_matching :pattern "*.rs")
  (filter :pattern "test")
  (sort_by "name")
  (unique)
  (count))
