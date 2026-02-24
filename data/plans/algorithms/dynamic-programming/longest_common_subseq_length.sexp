;; Longest common subsequence length between two strings
;; Example: LCS("abcde", "ace") = 3
;; expected: 3

(define (longest_common_subseq_length (a : String) (b : String))
  (bind a "abcde")
  (bind b "ace")
  (longest_common_subseq_length)
)
