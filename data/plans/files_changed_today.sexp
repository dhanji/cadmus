;; Files changed today: list files modified since midnight today
(define (files-changed-today (path : Dir))
  (find_modified_since :since "today")
  (sort_by :key "mtime"))