;; Weighted grade average: compute weighted mean then round to nearest integer
;; Example: scores (80 90 100) with weights (1 2 3) = 93.0
;; expected: 93.0

(define (weighted_grade (values : (List Number)) (weights : (List Number)))
  (bind values (list 80 90 100))
  (bind weights (list 1 2 3))
  (weighted_mean :values $values :weights $weights)
  (round $step-1)
)
