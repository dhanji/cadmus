;; Graham scan convex hull: find convex hull of points
;; Points: (0,0) (1,1) (2,2) (0,2) (2,0) (1,0) → hull size = 4
;; expected: 4

(define (graham_scan_convex_hull (n : Number))
  (bind n 6)
  (graham_scan_convex_hull)
)
