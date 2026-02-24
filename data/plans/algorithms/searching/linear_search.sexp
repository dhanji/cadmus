;; Linear search: find index of target in list, -1 if not found
;; Example: search 7 in (3 5 7 1 9) = 2
;; expected: 2

(define (linear_search (lst : (List Number)) (target : Number))
  (bind lst (list 3 5 7 1 9))
  (bind target 7)
  (linear_search)
)
