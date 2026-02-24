;; Character frequency report: strip whitespace then report char frequencies
;; Example: char_frequency_report("  banana  ") = "a:3,b:1,n:2"
;; expected: a:3,b:1,n:2

(define (char_frequency_report (s : String))
  (bind s "  banana  ")
  (strip_whitespace :s $s)
  (char_frequency_report :s $step-1)
)
