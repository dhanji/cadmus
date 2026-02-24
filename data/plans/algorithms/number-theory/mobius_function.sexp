;; Möbius function: μ(n) = 0 if squared prime factor, (-1)^k for k distinct primes
;; Example: μ(30) = 30=2×3×5, three distinct primes → (-1)^3 = -1
;; expected: -1

(define (mobius_function (n : Number))
  (bind n 30)
  (mobius_function)
)
