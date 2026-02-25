;; Add all unstaged changes and commit with a message
(define (commit-all (repo : Repo) (message : Name))
  (git_add :files ".")
  (git_commit :message "$message"))