;; Happy number check: repeatedly sum squares of digits until 1 or cycle
;; Example: 19 → 82 → 68 → 100 → 1 (happy!)
;; expected: #t

(define (happy_number_check (n : Number))
  (bind n 19)
  (happy_number_check)
)
