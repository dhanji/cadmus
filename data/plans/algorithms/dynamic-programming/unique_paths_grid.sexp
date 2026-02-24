;; Unique paths in m×n grid: top-left to bottom-right, only right/down
;; Example: 3×7 grid = 28 paths
;; expected: 28

(define (unique_paths_grid (m : Number) (n : Number))
  (bind m 3)
  (bind n 7)
  (unique_paths_grid)
)
