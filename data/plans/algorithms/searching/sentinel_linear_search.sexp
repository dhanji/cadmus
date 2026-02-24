;; Sentinel linear search: place target at end to avoid bounds check
;; Example: search 7 in (3 5 7 1 9) = 2
;; expected: 2

(define (sentinel_linear_search (lst : (List Number)) (target : Number))
  (bind lst (list 3 5 7 1 9))
  (bind target 7)
  (sentinel_linear_search)
)
