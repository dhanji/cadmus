;; Modular exponentiation: (base^exp) mod m using fast exponentiation
;; Example: (2^10) mod 1000 = 24
;; expected: 24

(define (modular_exponentiation (base : Number) (exp : Number) (m : Number))
  (bind base 2)
  (bind exp 10)
  (bind m 1000)
  (modular_exponentiation)
)
