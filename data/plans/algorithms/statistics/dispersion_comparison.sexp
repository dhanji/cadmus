;; Dispersion comparison: difference between standard deviation and mean absolute deviation
;; Example: for (2 4 4 4 5 5 7 9), stddev=2.0, MAD=1.5, diff=0.5
;; expected: 0.5

(define (dispersion_comparison (lst : (List Number)))
  (bind lst (list 2 4 4 4 5 5 7 9))
  (stddev_list :lst $lst)
  (mean_absolute_deviation :lst $lst)
  (subtract :x $step-1 :y $step-2)
)
