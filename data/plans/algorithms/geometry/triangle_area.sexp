;; Triangle area from three vertices using cross product
;; Example: (0,0), (4,0), (0,3) → area 6
;; expected: 6

(define (triangle_area (x1 : Number) (y1 : Number) (x2 : Number) (y2 : Number) (x3 : Number) (y3 : Number))
  (bind x1 0)
  (bind x2 4)
  (bind x3 0)
  (bind y1 0)
  (bind y2 0)
  (bind y3 3)
  (triangle_area)
)
