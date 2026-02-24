;; Birthday paradox: expected people for 50% collision probability in n-day year
;; Example: 365-day year → 23 people
;; expected: 23

(define (birthday_paradox_sim (n : Number))
  (bind n 365)
  (birthday_paradox_sim)
)
