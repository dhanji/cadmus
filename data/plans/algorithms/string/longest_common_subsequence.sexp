;; Longest common subsequence: find LCS of two strings
;; Example: lcs("ABCBDAB", "BDCAB") = 4
;; expected: 4

(define (longest_common_subsequence (s1 : String) (s2 : String))
  (bind s1 "ABCBDAB")
  (bind s2 "BDCAB")
  (longest_common_subsequence)
)
