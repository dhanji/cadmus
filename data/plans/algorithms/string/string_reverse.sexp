;; String reverse: reverse characters in a string
;; Example: reverse "hello" = "olleh"
;; expected: olleh

(define (string_reverse (s : String))
  (bind s "hello")
  (string_reverse)
)
