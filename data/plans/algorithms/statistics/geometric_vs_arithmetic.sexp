;; Geometric mean vs arithmetic mean: compute both and find the difference
;; Example: for (1 2 4 8), arithmetic=3.75, geometric=2.828..., diff=0.921...
;; expected: 0.9215728752538097

(define (geometric_vs_arithmetic (lst : (List Number)))
  (bind lst (list 1 2 4 8))
  (mean_list :lst $lst)
  (geometric_mean :lst $lst)
  (subtract :x $step-1 :y $step-2)
)
