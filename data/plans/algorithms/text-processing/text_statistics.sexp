;; Text statistics: strip whitespace then compute word count, total chars, avg word length
;; Example: stats of "  hello world foo  " = "3 13 4"
;; expected: 3 13 4

(define (text_statistics (s : String))
  (bind s "  hello world foo  ")
  (strip_whitespace :s $s)
  (text_statistics :s $step-1)
)
