;; Standard error of the mean: stddev divided by square root of sample size
;; Example: standard_error of (2 4 4 4 5 5 7 9) = 0.7071067811865475
;; expected: 0.7071067811865475

(define (standard_error (lst : (List Number)))
  (bind lst (list 2 4 4 4 5 5 7 9))
  (stddev_list :lst $lst)
  (length $lst)
  (sqrt $step-2)
  (divide :x $step-1 :y $step-3)
)
