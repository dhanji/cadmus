;; Permutation count: P(n,k) = n! / (n-k)!
;; Example: P(5, 3) = 60
;; expected: 60

(define (permutation_count (n : Number) (k : Number))
  (bind n 5)
  (bind k 3)
  (permutation_count)
)
