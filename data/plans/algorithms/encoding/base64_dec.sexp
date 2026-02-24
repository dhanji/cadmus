;; Base64 decode: decode base64 string to original
;; Example: decode "SGVsbG8=" = "Hello"
;; expected: Hello

(define (base64_dec (s : String))
  (bind s "SGVsbG8=")
  (base64_dec)
)
