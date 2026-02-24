;; Ternary search: find target in sorted list using three-way split
;; Example: search 7 in (1 3 5 7 9 11 13) = 3
;; expected: 3

(define (ternary_search (lst : (List Number)) (target : Number))
  (bind lst (list 1 3 5 7 9 11 13))
  (bind target 7)
  (ternary_search)
)
