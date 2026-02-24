;; Copy and organize: walk, filter PDFs, sort
(define (copy-and-organize (source : Dir) (dest : Dir) (pattern : Pattern))
  (walk_tree)
  (filter :extension ".pdf")
  (sort_by "name"))
