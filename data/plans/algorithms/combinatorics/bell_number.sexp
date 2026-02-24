;; Bell number: B(n) counts partitions of a set of n elements
;; Example: B(5) = 52
;; expected: 52

(define (bell_number (n : Number))
  (bind n 5)
  (bell_number)
)
