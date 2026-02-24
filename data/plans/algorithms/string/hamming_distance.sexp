;; Hamming distance: count positions where strings differ
;; Example: hamming("karolin", "kathrin") = 3
;; expected: 3

(define (hamming_distance (s1 : String) (s2 : String))
  (bind s1 "karolin")
  (bind s2 "kathrin")
  (hamming_distance)
)
