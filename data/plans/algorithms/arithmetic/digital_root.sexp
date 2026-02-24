;; Digital root: repeatedly sum digits until single digit
;; Example: digital_root(493) = 7 (4+9+3=16, 1+6=7)
;; expected: 7

(define (digital_root (n : Number))
  (bind n 493)
  (digital_root)
)
