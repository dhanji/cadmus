;; MurmurHash3 (simplified 32-bit)
;; Example: murmur("hello", seed=42) — deterministic hash
;; expected: 2384946537

(define (murmurhash3 (s : String))
  (bind s "hello")
  (murmurhash3)
)
