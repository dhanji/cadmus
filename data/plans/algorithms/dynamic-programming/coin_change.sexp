;; Coin change: minimum coins to make amount
;; Example: coins (1 5 10 25), amount 36 = 3 (25+10+1)
;; expected: 3

(define (coin_change (amount : Number))
  (bind amount 36)
  (coin_change)
)
