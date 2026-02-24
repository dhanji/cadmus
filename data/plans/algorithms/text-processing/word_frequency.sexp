;; Word frequency: count occurrences of a target word in text
;; Example: count "the" in "the cat sat on the mat the cat" = 3
;; expected: 3

(define (word_frequency (text : String) (target : String))
  (bind text "the cat sat on the mat the cat")
  (bind target "the")
  (word_frequency)
)
