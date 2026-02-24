;; Interval scheduling max: maximum non-overlapping intervals
;; Example: ((1 2) (2 3) (3 4) (1 3)) → 3 non-overlapping
;; expected: 3

(define (interval_scheduling_max (intervals : (List (List Number))))
  (bind intervals (list (list 1 2) (list 2 3) (list 3 4) (list 1 3)))
  (interval_scheduling_max)
)
