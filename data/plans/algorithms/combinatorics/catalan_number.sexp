;; Catalan number: C(n) = C(2n,n)/(n+1)
;; Example: C(5) = 42
;; expected: 42

(define (catalan_number (n : Number))
  (bind n 5)
  (catalan_number)
)
