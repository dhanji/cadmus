;; Git activity report: log, filter, sort, deduplicate, count
(define (git-activity (repo : Repo) (pattern : Pattern))
  (git_log)
  (filter :pattern "$pattern")
  (sort_by "date")
  (unique)
  (count))
