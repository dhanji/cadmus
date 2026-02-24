;; CSV column sum: parse CSV rows and sum a numeric column
;; Example: sum column 1 of "1,10\n2,20\n3,30" = 60
;; expected: 60

(define (csv_column_sum (data : String) (col : Number))
  (bind data "1,10\n2,20\n3,30")
  (bind col 1)
  (csv_column_sum)
)
