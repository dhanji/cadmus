;; Find large files in a directory
(define (find-large-files (path : Dir) (min_size : Size))
  (list_dir)
  (sort_by "name"))
