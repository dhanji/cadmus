;; Merge overlapping intervals
;; Example: ((1 3) (2 6) (8 10) (15 18)) → ((15 18) (8 10) (1 6))
;; expected: ((15 18) (8 10) (1 6))

(define (merge_intervals (intervals : (List (List Number))))
  (bind intervals (list (list 1 3) (list 2 6) (list 8 10) (list 15 18)))
  (merge_intervals)
)
