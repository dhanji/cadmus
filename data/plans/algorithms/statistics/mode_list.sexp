;; Mode of a list of numbers: find the most frequent value
;; Example: mode_list((1 2 2 3 3 3 4)) = 3
;; expected: 3

(define (mode_list (lst : (List Number)))
  (bind lst (list 1 2 2 3 3 3 4))
  (mode_list)
)
