;; Wagner-Fischer: edit distance (same as Levenshtein)
;; Example: wf("sunday", "saturday") = 3
;; expected: 3

(define (wagner_fischer (s1 : String) (s2 : String))
  (bind s1 "sunday")
  (bind s2 "saturday")
  (wagner_fischer)
)
