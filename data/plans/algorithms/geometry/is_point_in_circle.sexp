;; Check if point (px,py) is inside circle centered at (cx,cy) with radius r
;; Example: (1,1) inside circle at (0,0) radius 2 → true
;; expected: #t

(define (is_point_in_circle (px : Number) (py : Number) (cx : Number) (cy : Number) (r : Number))
  (bind cx 0)
  (bind cy 0)
  (bind px 1)
  (bind py 1)
  (bind r 2)
  (is_point_in_circle)
)
