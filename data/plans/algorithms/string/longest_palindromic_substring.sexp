;; Longest palindromic substring using expand-around-center
;; Example: "babad" → "bab"
;; expected: bab

(define (longest_palindromic_substring (s : String))
  (bind s "babad")
  (longest_palindromic_substring)
)
