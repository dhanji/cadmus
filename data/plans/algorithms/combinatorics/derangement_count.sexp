;; Derangement count: D(n) = (n-1)(D(n-1) + D(n-2))
;; Example: D(5) = 44
;; expected: 44

(define (derangement_count (n : Number))
  (bind n 5)
  (derangement_count)
)
