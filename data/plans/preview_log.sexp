;; Preview log: walk directory, find .log files, sort
(define (preview-log (path : Dir) (keyword : Pattern))
  (walk_tree)
  (filter :extension ".log")
  (sort_by "name"))
