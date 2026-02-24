;; CSV row parsing: split a comma-separated row into fields
;; Example: csv_parse_row("a,b,c") = (a b c)
;; expected: (a b c)

(define (csv_parse_row (row : String))
  (bind row "a,b,c")
  (csv_parse_row)
)
