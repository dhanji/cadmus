;; Next power of two: smallest 2^k >= n
;; Example: next power of two for 13 = 16
;; expected: 16

(define (next_power_of_two (n : Number))
  (bind n 13)
  (next_power_of_two)
)
