;; Closest pair of points: minimum distance between any two points
;; Points: (0,0) (3,4) (1,1) (5,5) (2,2) → closest = sqrt(2) ≈ 1.4142
;; expected: 1.4142135623730951

(define (closest_pair_of_points (pts : (List Any)))
  (bind pts (list (cons 0 0) (cons 3 4) (cons 1 1) (cons 5 5) (cons 2 2)))
  (closest_pair_of_points)
)
