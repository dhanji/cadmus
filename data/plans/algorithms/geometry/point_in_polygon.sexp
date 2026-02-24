;; Point in polygon: ray casting algorithm
;; Example: point (1,1) in square (0,0)(4,0)(4,4)(0,4) = #t
;; expected: #t

(define (point_in_polygon (n : Number))
  (bind n 4)
  (point_in_polygon)
)
