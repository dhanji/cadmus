;; DJB2 hash: simple string hash function by Dan Bernstein
;; Example: djb2("hello") = 210714636441
;; expected: 210714636441

(define (djb2_hash (s : String))
  (bind s "hello")
  (djb2_hash)
)
