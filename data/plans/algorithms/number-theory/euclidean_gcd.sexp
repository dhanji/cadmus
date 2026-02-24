;; Euclidean GCD: gcd(a,b) using Euclid's algorithm
;; Example: gcd(48, 18) = 6
;; expected: 6

(define (euclidean_gcd (a : Number) (b : Number))
  (bind a 48)
  (bind b 18)
  (euclidean_gcd)
)
