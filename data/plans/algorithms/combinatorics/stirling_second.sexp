;; Stirling number of the second kind: S(n,k) partitions of n into k non-empty subsets
;; Example: S(5,3) = 25
;; expected: 25

(define (stirling_second (n : Number) (k : Number))
  (bind k 3)
  (bind n 5)
  (stirling_second)
)
