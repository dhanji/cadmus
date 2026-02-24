;; Outlier detection: count outliers and compute their percentage of total
;; Example: (10 10 10 10 10 10 10 10 50 -30) with threshold 2, outliers=2, pct=20.0
;; expected: 20

(define (outlier_percentage (lst : (List Number)) (threshold : Number))
  (bind lst (list 10 10 10 10 10 10 10 10 50 -30))
  (bind threshold 2)
  (outlier_count :lst $lst :threshold $threshold)
  (length $lst)
  (divide :x $step-1 :y $step-2)
  (multiply :x $step-3 :y 100)
)
