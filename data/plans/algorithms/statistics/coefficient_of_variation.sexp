;; Coefficient of variation: standard deviation divided by mean times 100
;; Example: cv of (2 4 4 4 5 5 7 9) = 40.0
;; expected: 40.0

(define (coefficient_of_variation (lst : (List Number)))
  (bind lst (list 2 4 4 4 5 5 7 9))
  (mean_list :lst $lst)
  (stddev_list :lst $lst)
  (divide :x $step-2 :y $step-1)
  (multiply :x $step-3 :y 100)
)
