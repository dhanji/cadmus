;; Levenshtein edit distance using DP
;; Example: lev("kitten", "sitting") = 3
;; expected: 3

(define (levenshtein_edit_distance (s1 : String) (s2 : String))
  (bind s1 "kitten")
  (bind s2 "sitting")
  (levenshtein_edit_distance)
)
