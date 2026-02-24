;; Git log search: search commit history for a pattern
(define (git-log-search (repo : Repo) (pattern : Pattern))
  (git_log)
  (filter :pattern "$pattern")
  (sort_by "date"))
