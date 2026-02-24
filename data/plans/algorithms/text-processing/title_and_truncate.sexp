;; Title and truncate: strip whitespace, title-case, then truncate
;; Example: "  hello world example  " → "Hello World ..."
;; expected: Hello World ...

(define (title_and_truncate (s : String) (max_len : Number))
  (bind max_len 15)
  (bind s "  hello world example  ")
  (strip_whitespace :s $s)
  (title_case :s $step-1)
  (truncate_string :s $step-2 :max_len $max_len)
)
