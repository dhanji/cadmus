;; Monte Carlo Pi estimation using fixed seed
;; Use deterministic sequence instead of random for reproducibility
;; expected: 3.17

(define (monte_carlo_pi (n : Number))
  (bind n 10000)
  (monte_carlo_pi)
)
