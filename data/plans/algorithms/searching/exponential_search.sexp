;; Exponential search: find range then binary search
;; Example: search 7 in (1 3 5 7 9 11 13) = 3
;; expected: 3

(define (exponential_search (lst : (List Number)) (target : Number))
  (bind lst (list 1 3 5 7 9 11 13))
  (bind target 7)
  (exponential_search)
)
