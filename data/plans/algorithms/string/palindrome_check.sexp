;; Palindrome check: is string a palindrome?
;; Example: "racecar" = #t
;; expected: #t

(define (palindrome_check (s : String))
  (bind s ""racecar"")
  (string_to_list $s)
  (reverse_list $step-1)
  (list_to_string $step-2)
  (string_equal :x $s :y $step-3)
)
