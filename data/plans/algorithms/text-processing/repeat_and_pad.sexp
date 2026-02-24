;; Repeat and pad: repeat a string n times then left-pad to width
;; Example: repeat "ab" 3 times, pad to 10 with "." = "....ababab"
;; expected: ....ababab

(define (repeat_and_pad (s : String) (n : Number) (width : Number))
  (bind n 3)
  (bind s "ab")
  (bind width 10)
  (repeat_string :s $s :n $n)
  (pad_left :s $step-1 :width $width :pad_char ".")
)
