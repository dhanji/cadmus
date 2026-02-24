;; Legendre symbol (a/p): 0 if p|a, 1 if a is QR mod p, -1 otherwise
;; Example: (2/7) = 2^3 mod 7 = 1 → quadratic residue
;; expected: 1

(define (legendre_symbol (a : Number) (p : Number))
  (bind a 2)
  (bind p 7)
  (legendre_symbol)
)
