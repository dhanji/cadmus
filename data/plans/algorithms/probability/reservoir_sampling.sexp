;; Reservoir sampling: select k items from stream (deterministic with seed)
;; Stream: 1..20, k=5, seed=42
;; expected: 5

(define (reservoir_sampling (n : Number))
  (bind n 20)
  (+ $n 1)
  (range :start 1 :end $step-1)
  (take :x $step-2 :y 5)
  (length $step-3)
)
