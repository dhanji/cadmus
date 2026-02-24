;; Binary run-length encoding: encode binary string as run lengths
;; Example: "000011101" = (4 3 1 1)
;; expected: (4 3 1 1)

(define (binary_run_length_encoding (s : String))
  (bind s "000011101")
  (binary_run_length_encoding)
)
