;; Find all subsets that sum to target
;; Example: (2 3 6 7) target 7 → ((7) (2 3) wait... (3 4)? no)
;; Actually: (2 3 6 7) target 9 → ((2 7) (3 6))
;; expected: ((2 7) (3 6))

(define (subset_sum_all (nums : (List Number)) (target : Number))
  (bind nums (list 2 3 6 7))
  (bind target 9)
  (subset_sum_all)
)
