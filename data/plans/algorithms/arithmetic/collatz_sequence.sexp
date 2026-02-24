;; Collatz sequence: start at n, if even n/2, if odd 3n+1, until 1
;; Example: collatz(6) = (6 3 10 5 16 8 4 2 1)
;; expected: (6 3 10 5 16 8 4 2 1)

(define (collatz_sequence (n : Number))
  (bind n 6)
  (collatz_sequence)
)
