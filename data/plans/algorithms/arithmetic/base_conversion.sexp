;; Base conversion: convert decimal to given base
;; Example: 255 in base 16 = "FF"
;; expected: FF

(define (base_conversion (n : Number) (base : Number))
  (bind base 16)
  (bind n 255)
  (base_conversion)
)
