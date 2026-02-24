;; Variance and standard deviation of a list of numbers
;; Example: compute variance then stddev of (2 4 4 4 5 5 7 9) = 2.0
;; expected: 2.0

(define (variance_and_stddev (lst : (List Number)))
  (bind lst (list 2 4 4 4 5 5 7 9))
  (variance_list :lst $lst)
  (stddev_list :lst $lst)
)
