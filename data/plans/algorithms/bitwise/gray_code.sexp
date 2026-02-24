;; Gray code: generate n-bit Gray code sequence
;; Example: gray(3) = (0 1 3 2 6 7 5 4)
;; expected: (0 1 3 2 6 7 5 4)

(define (gray_code (n : Number))
  (bind n 3)
  (gray_code)
)
