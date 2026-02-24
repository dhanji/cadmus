;; Sum of all divisors of n (including n)
;; Example: σ(12) = 1+2+3+4+6+12 = 28
;; expected: 28

(define (sum_of_divisors (n : Number))
  (bind n 12)
  (sum_of_divisors)
)
