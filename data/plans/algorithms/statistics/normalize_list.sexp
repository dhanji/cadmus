;; Normalize a list by computing z-scores for each element
;; Example: normalize (2 4 4 4 5 5 7 9) gives z-scores
;; expected: (-1.5 -0.5 -0.5 -0.5 0.0 0.0 1.0 2.0)

(define (normalize_list (lst : (List Number)))
  (bind lst (list 2 4 4 4 5 5 7 9))
  (normalize_list)
)
