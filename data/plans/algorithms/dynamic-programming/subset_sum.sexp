;; Subset sum: can a subset sum to target?
;; Example: (3 34 4 12 5 2), target 9 = #t (4+5)
;; expected: #t

(define (subset_sum (target : Number))
  (bind target 9)
  (subset_sum)
)
