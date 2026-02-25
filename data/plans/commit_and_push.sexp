;; Add all changes, commit, and push to remote
(define (commit-and-push (repo : Repo) (message : Name))
  (git_add :files ".")
  (git_commit :message "$message")
  (git_push :repo "$repo" :remote "origin" :branch "main"))