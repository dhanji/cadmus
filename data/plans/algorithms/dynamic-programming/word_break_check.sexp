;; Word break: can string be segmented into dictionary words?
;; Example: "leetcode" with dict ("leet" "code") → true
;; expected: #t

(define (word_break_check (s : String) (dict : (List String)))
  (bind dict (list "leet" "code"))
  (bind s ""leetcode"")
  (word_break_check)
)
