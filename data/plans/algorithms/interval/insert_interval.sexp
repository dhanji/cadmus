;; Insert interval into sorted non-overlapping list, merging as needed
;; Example: ((1 3) (6 9)) insert (2 5) → ((1 5) (6 9))
;; expected: ((1 5) (6 9))

(define (insert_interval (intervals : (List (List Number))) (new_iv : (List Number)))
  (bind intervals (list (list 1 3) (list 6 9)))
  (bind new_iv (list 2 5))
  (insert_interval)
)
