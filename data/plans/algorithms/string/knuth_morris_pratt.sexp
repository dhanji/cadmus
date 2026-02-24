;; KMP string search: find first occurrence of pattern in text
;; Example: kmp("ABABDABACDABABCABAB", "ABABCABAB") = 9
;; expected: 10

(define (knuth_morris_pratt (text : String) (pattern : String))
  (bind pattern "ABABCABAB")
  (bind text "ABABDABACDABABCABAB")
  (knuth_morris_pratt)
)
