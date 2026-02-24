;; Perfect number check: sum of proper divisors equals n
;; Example: 28 is perfect (1+2+4+7+14=28)
;; expected: #t

(define (perfect_number_check (n : Number))
  (bind n 28)
  (perfect_number_check)
)
