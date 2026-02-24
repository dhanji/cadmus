;; 0/1 Knapsack: maximize value within weight capacity
;; Items: (weight, value) = (1,1) (3,4) (4,5) (5,7), capacity 7
;; Example: max value = 9 (items 2+3: weight 3+4=7, value 4+5=9)
;; expected: 9

(define (zero_one_knapsack (capacity : Number))
  (bind capacity 7)
  (zero_one_knapsack)
)
