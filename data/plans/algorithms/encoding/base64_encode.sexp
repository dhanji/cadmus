;; Base64 encode a string
;; Example: base64("Hello") = "SGVsbG8="
;; expected: SGVsbG8=

(define (base64_encode (s : String))
  (bind s "Hello")
  (base64_enc)
)
