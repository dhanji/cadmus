;; Euler's totient: count integers 1..n coprime to n
;; expected: 4

(define (euler_totient (n : Number)) : Number
  (bind n 12)
  (for/fold ([total 0]) ([i (range 1 n)])
    (cond
      [(= (gcd i n) 1) (+ total 1)]
      [else total])))
