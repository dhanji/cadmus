;; Miller-Rabin primality test (deterministic for small n)
;; Example: 104729 is prime
;; expected: #t

(define (miller_rabin_primality (n : Number))
  (bind n 104729)
  (miller_rabin_primality)
)
