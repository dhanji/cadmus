;; Bit parity: count parity of set bits
;; Example: 7 = 111 has 3 set bits (odd) → 1
;; expected: 1

(define (bit_parity (n : Number))
  (bind n 7)
  (bit_parity)
)
