;; Generate parentheses: all valid combinations of n pairs
;; Example: n=3 → ("((()))" "(()())" "(())()" "()(())" "()()()")
;; expected: (((())) (()()) (())() ()(()) ()()())

(define (generate_parentheses (n : Number))
  (bind n 3)
  (generate_parentheses)
)
