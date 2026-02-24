;; Variable-length quantity encoding
;; Example: vlq(300) = (2 44) — 300 = 0x12C → groups of 7 bits
;; expected: (172 2)

(define (vlq_encoding (n : Number))
  (bind n 300)
  (vlq_encoding)
)
