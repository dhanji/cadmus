;; Cleanup temp files: find .tmp files for review
(define (cleanup-temp (path : Dir))
  (walk_tree)
  (filter :pattern "*.tmp")
  (sort_by "name"))
