;; Damerau-Levenshtein distance (with transpositions)
;; Example: dl("ca", "abc") = 2
;; expected: 3

(define (damerau_levenshtein (s1 : String) (s2 : String))
  (bind s1 "ca")
  (bind s2 "abc")
  (damerau_levenshtein)
)
