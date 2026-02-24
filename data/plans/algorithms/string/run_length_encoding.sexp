;; Run-length encoding: compress consecutive chars
;; Example: rle("aaabbc") = "3a2b1c"
;; expected: 3a2b1c

(define (run_length_encoding (s : String))
  (bind s "aaabbc")
  (run_length_encoding)
)
