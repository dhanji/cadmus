;; Triangular number: T(n) = n*(n+1)/2
;; Example: T(10) = 55
;; expected: 55

(define (triangular_number (n : Number))
  (bind n 10)
  (triangular_number)
)
