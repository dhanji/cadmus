;; Manacher's algorithm: longest palindromic substring length
;; Example: longest palindrome in "babad" = 3 ("bab" or "aba")
;; expected: 3

(define (manacher_longest_palindrome (s : String))
  (bind s "babad")
  (manacher_longest_palindrome)
)
