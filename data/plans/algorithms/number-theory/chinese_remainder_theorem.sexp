;; Chinese Remainder Theorem: solve system of congruences
;; x ≡ 2 (mod 3), x ≡ 3 (mod 5), x ≡ 2 (mod 7) → x = 23
;; expected: 23

(define (chinese_remainder_theorem (n : Number))
  (bind n 0)
  (chinese_remainder_theorem)
)
