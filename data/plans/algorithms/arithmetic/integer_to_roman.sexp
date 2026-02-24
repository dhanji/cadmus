;; Integer to roman: convert decimal to Roman numeral
;; Example: 1994 = MCMXCIV
;; expected: MCMXCIV

(define (integer_to_roman (n : Number))
  (bind n 1994)
  (integer_to_roman)
)
