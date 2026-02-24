;; Longest repeating substring using suffix array
;; Example: "banana" → "ana"
;; expected: ana

(define (longest_repeating_substring (s : String))
  (bind s "banana")
  (longest_repeating_substring)
)
