;; Sample variance: population variance times n/(n-1) for Bessel correction
;; Example: sample variance of (2 4 4 4 5 5 7 9) = 4.571428571428571
;; expected: 4.571428571428571

(define (sample_variance (lst : (List Number)))
  (bind lst (list 2 4 4 4 5 5 7 9))
  (variance_list :lst $lst)
  (length $lst)
  (subtract :x $step-2 :y 1)
  (divide :x $step-2 :y $step-3)
  (multiply :x $step-1 :y $step-4)
)
