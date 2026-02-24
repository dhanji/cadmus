;; Harmonic mean vs arithmetic mean: compute both and find the difference
;; Example: for (1 2 4), arithmetic=2.333..., harmonic=1.714..., diff=0.619...
;; expected: 0.6190476190476193

(define (harmonic_vs_arithmetic (lst : (List Number)))
  (bind lst (list 1 2 4))
  (mean_list :lst $lst)
  (harmonic_mean :lst $lst)
  (subtract :x $step-1 :y $step-2)
)
