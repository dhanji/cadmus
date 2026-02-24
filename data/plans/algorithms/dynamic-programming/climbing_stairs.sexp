;; Climbing stairs: ways to climb n stairs (1 or 2 steps at a time)
;; Example: stairs(10) = 89
;; expected: 89

(define (climbing_stairs (n : Number))
  (bind n 10)
  (climbing_stairs)
)
