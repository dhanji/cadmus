;; Archive source files: walk, find .rs files, sort, pack
(define (archive-sources (path : Dir))
  (walk_tree)
  (find_matching :pattern "*.rs")
  (sort_by "name")
  (pack_archive))
