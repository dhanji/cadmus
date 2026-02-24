;; Catalan number: C(n) = (2n choose n) / (n+1)
;; Example: C(5) = 42
;; expected: 42

(define (catalan_number_compute (n : Number))
  (bind n 5)
  (catalan_number_compute)
)
