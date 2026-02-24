;; Word count: count the number of words in a string
;; Example: word_count("the quick brown fox") = 4
;; expected: 4

(define (word_count (s : String))
  (bind s "the quick brown fox")
  (word_count)
)
