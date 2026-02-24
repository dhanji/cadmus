;; Coprimality test: gcd(a,b) = 1?
;; Example: coprime(15, 28) = #t
;; expected: #t

(define (coprimality_test (a : Number) (b : Number))
  (bind a 15)
  (bind b 28)
  (gcd :x $a :y $b)
  (equal :x $step-1 :y 1)
)
