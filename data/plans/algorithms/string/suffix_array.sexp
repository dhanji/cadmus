;; Suffix array: sorted array of all suffix indices
;; Example: "banana" → (5 3 1 0 4 2)
;; expected: (5 3 1 0 4 2)

(define (suffix_array (s : String))
  (bind s "banana")
  (suffix_array)
)
