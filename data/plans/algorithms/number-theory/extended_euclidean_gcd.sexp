;; Extended Euclidean GCD: find x,y such that ax + by = gcd(a,b)
;; Example: ext_gcd(48, 18) = (6 -1 3) meaning 48*(-1) + 18*3 = 6
;; expected: (6 -1 3)

(define (extended_euclidean_gcd (a : Number) (b : Number))
  (bind a 48)
  (bind b 18)
  (extended_euclidean_gcd)
)
