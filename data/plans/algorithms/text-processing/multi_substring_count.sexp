;; Multi-string substring count: count total occurrences of a pattern across comma-separated strings
;; Example: count "abc" in "abcabc,xyzxyzxyz,abcxyz" = 3
;; expected: 3

(define (multi_substring_count (data : String) (sub : String))
  (bind data "abcabc,xyzxyzxyz,abcxyz")
  (bind sub "abc")
  (multi_substring_count)
)
