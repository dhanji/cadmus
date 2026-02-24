;; Primality test by trial division
;; Example: is_prime(97) = #t
;; expected: #t

(define (primality_trial_division (n : Number))
  (bind n 97)
  (primality_trial_division)
)
