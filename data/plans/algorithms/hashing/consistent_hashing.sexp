;; Consistent hashing: assign keys to nodes on a ring
;; 3 nodes at positions 100, 200, 300; key at position 150 → node 200
;; expected: 200

(define (consistent_hashing (n : Number))
  (bind n 0)
  (consistent_hashing)
)
