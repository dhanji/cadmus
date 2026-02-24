;; Floyd's cycle detection: find cycle start in linked list
;; List: 1→2→3→4→5→3 (cycle at index 2)
;; expected: 3

(define (floyd_cycle_detection (n : Number))
  (bind n 6)
  (floyd_cycle_detection)
)
