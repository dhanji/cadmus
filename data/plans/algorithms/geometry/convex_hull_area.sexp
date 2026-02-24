;; Convex hull area: compute area of convex hull of points
;; Example: points (0,0) (4,0) (4,3) (0,3) → area 12
;; expected: 12

(define (convex_hull_area (points : (List (List Number))))
  (bind points (list (list 0 0) (list 4 0) (list 4 3) (list 0 3) (list 2 1)))
  (convex_hull_area)
)
