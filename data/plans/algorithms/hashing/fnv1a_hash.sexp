;; FNV-1a hash (32-bit)
;; Example: fnv1a("hello") = 1335831723
;; expected: 1335831723

(define (fnv1a_hash (s : String))
  (bind s "hello")
  (fnv1a_hash)
)
