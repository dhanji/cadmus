;; Show disk usage for the home directory sorted largest first
(define (disk-usage (path : Dir))
  (du_size :path "$path")
  (sort_by :key "size"))