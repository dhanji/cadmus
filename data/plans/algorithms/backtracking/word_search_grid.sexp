;; Word search: find word in a 2D character grid
;; Grid: A B C E / S F C S / A D E E
;; Example: find "ABCCED" → true
;; expected: #t

(define (word_search_grid (word : String))
  (bind word ""ABCCED"")
  (word_search_grid)
)
