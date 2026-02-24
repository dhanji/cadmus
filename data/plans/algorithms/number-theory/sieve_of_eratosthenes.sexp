;; Sieve of Eratosthenes: find all primes up to n
;; Example: primes up to 30 = (2 3 5 7 11 13 17 19 23 29)
;; expected: (2 3 5 7 11 13 17 19 23 29)

(define (sieve_of_eratosthenes (n : Number))
  (bind n 30)
  (sieve_of_eratosthenes)
)
