;; Interval intersection: find overlapping ranges
;; Example: A=((0 2) (5 10) (13 23) (24 25)) B=((1 5) (8 12) (15 24) (25 26))
;; → ((1 2) (5 5) (8 10) (15 23) (24 24) (25 25))
;; expected: ((1 2) (5 5) (8 10) (15 23) (24 24) (25 25))

(define (interval_intersection (a : (List (List Number))) (b : (List (List Number))))
  (bind a (list (list 0 2) (list 5 10) (list 13 23) (list 24 25)))
  (bind b (list (list 1 5) (list 8 12) (list 15 24) (list 25 26)))
  (interval_intersection)
)
