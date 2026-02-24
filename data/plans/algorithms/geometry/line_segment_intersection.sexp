;; Line segment intersection: do segments (0,0)-(2,2) and (0,2)-(2,0) intersect?
;; Example: intersect? = #t
;; expected: #t

(define (line_segment_intersection (n : Number))
  (bind n 0)
  (line_segment_intersection)
)
