;; Integer partition count: number of ways to write n as sum of positive integers
;; Example: p(10) = 42
;; expected: 42

(define (integer_partition_count (n : Number))
  (bind n 10)
  (integer_partition_count)
)
