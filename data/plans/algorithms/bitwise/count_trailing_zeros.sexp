;; Count trailing zeros in binary representation
;; Example: 40 = 101000 has 3 trailing zeros
;; expected: 3

(define (count_trailing_zeros (n : Number))
  (bind n 40)
  (count_trailing_zeros)
)
