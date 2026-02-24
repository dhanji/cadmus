;; ROT13: Caesar cipher with shift 13
;; Example: rot13("Hello") = "Uryyb"
;; expected: Uryyb

(define (rot13_cipher (s : String))
  (bind s "Hello")
  (rot13_cipher)
)
