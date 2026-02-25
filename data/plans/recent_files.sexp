;; Recent files: list recently modified files across common user directories
(define (recent-files (path : Dir))
  (find_recent :dir "$path" :days "7")
  (sort_by :key "mtime"))