;; Longest palindromic subsequence length
;; Example: LPS of "bbbab" = 4
;; expected: 4

(define (longest_palindromic_subsequence (s : String))
  (bind s "bbbab")
  (longest_palindromic_subsequence)
)
