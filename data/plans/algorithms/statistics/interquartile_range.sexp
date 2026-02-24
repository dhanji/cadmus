;; Interquartile range: difference between 75th and 25th percentiles
;; Example: IQR of (1 2 3 4 5 6 7 8 9 10) = 4.5
;; expected: 4.5

(define (interquartile_range (lst : (List Number)))
  (bind lst (list 1 2 3 4 5 6 7 8 9 10))
  (percentile :lst $lst :p 25)
  (percentile :lst $lst :p 75)
  (subtract :x $step-2 :y $step-1)
)
