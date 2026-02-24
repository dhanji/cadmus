;; Project health check: walk, find .rs, filter tests, sort, unique, count
(define (project-health (path : Dir))
  (walk_tree)
  (find_matching :pattern "*.rs")
  (filter :pattern "test")
  (sort_by "name")
  (unique)
  (count))
