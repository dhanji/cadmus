;; Shoelace formula: area of polygon given vertices
;; Example: triangle (0,0) (4,0) (0,3) → area = 6
;; expected: 6

(define (shoelace_polygon_area (n : Number))
  (bind n 3)
  (shoelace_polygon_area)
)
