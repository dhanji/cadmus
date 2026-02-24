;; Caesar cipher: shift each letter by k positions
;; Example: caesar("HELLO", 3) = "KHOOR"
;; expected: KHOOR

(define (caesar_cipher (s : String) (k : Number))
  (bind k 3)
  (bind s "HELLO")
  (caesar_cipher)
)
