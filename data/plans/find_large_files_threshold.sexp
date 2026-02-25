;; Find large files threshold: find files larger than a size threshold (default 100MB)
(define (find-large-files-threshold (path : Dir) (min_size : Size))
  (find_by_size :min_size "$min_size")
  (sort_by :key "size"))