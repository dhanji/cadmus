;; Pascal's triangle row: row n (0-indexed)
;; Example: row 5 = (1 5 10 10 5 1)
;; expected: (1 5 10 10 5 1)

(define (pascals_triangle_row (n : Number))
  (bind n 5)
  (pascals_triangle_row)
)
