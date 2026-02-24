;; Mean minus median: measure of skewness direction
;; Example: mean_minus_median of (1 2 3 4 100) = 22.0 - 3 = 19.0
;; expected: 19.0

(define (mean_minus_median (lst : (List Number)))
  (bind lst (list 1 2 3 4 100))
  (mean_list :lst $lst)
  (median_list :lst $lst)
  (subtract :x $step-1 :y $step-2)
)
