;; Longest repeating substring of a given string
;; Example: "banana" → "ana"
;; expected: ana

(define (longest_repeating_substring (s : String))
  (bind s "banana")
  (longest_repeating_substring)
)