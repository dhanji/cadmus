;; Prime factorization: list prime factors of n
;; Example: factors(84) = (2 2 3 7)
;; expected: (2 2 3 7)

(define (prime_factorization (n : Number))
  (bind n 84)
  (prime_factorization)
)
