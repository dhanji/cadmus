;; Arithmetic mean of a list of numbers
;; Example: mean_list((1 2 3 4 5)) = 3.0
;; expected: 3.0

(define (mean_list (lst : (List Number)))
  (bind lst (list 1 2 3 4 5))
  (mean_list)
)
