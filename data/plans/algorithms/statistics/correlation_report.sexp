;; Pearson correlation coefficient of two data series
;; Example: correlation of (1 2 3 4 5) and (2 4 5 4 5) = 0.7745966692414833
;; expected: 0.7745966692414833

(define (correlation_report (xs : (List Number)) (ys : (List Number)))
  (bind xs (list 1 2 3 4 5))
  (bind ys (list 2 4 5 4 5))
  (covariance :xs $xs :ys $ys)
  (correlation :xs $xs :ys $ys)
)
