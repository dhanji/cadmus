;; Five number summary: min, Q1, median, Q3, max of a list
;; Example: five_number_summary of (1 2 3 4 5 6 7 8 9 10) = 1 3.25 5.5 7.75 10
;; expected: 1 3.25 5.5 7.75 10

(define (five_number_summary (lst : (List Number)))
  (bind lst (list 1 2 3 4 5 6 7 8 9 10))
  (five_number_summary)
)
