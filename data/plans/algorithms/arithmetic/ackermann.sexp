;; Ackermann function: A(m,n) — fast-growing recursive function
;; Example: A(3,2) = 29
;; expected: 29

(define (ackermann (m : Number) (n : Number))
  (bind m 3)
  (bind n 2)
  (ackermann)
)
