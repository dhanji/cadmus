;; Spotlight find: search with Spotlight, sort results
(define (spotlight-find (query : Pattern))
  (spotlight_search)
  (sort_by "name"))
