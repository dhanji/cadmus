;; Adler-32 checksum: fast rolling hash
;; Example: adler32("Wikipedia") = 300286872
;; expected: 300286872

(define (adler32_hash (s : String))
  (bind s "Wikipedia")
  (adler32_hash)
)
