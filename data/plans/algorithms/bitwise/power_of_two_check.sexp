;; Power of two check: n > 0 and n & (n-1) = 0
;; Example: is_pow2(64) = #t
;; expected: #t

(define (power_of_two_check (n : Number))
  (bind n 64)
  (subtract :x $n :y 1)
  (bitwise_and :x $n :y $step-1)
  (equal :x $step-2 :y 0)
  (and_logic :x (greater_than :x $n :y 0) :y $step-3)
)
