;; Z-algorithm: compute Z-array for string matching
;; Example: "aabxaa" → (6 1 0 0 2 1)
;; expected: (6 1 0 0 2 1)

(define (z_algorithm (s : String))
  (bind s "aabxaa")
  (z_algorithm)
)
