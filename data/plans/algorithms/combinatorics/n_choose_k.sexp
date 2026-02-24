;; Binomial coefficient: C(n,k) = n! / (k! * (n-k)!)
;; Example: C(10, 3) = 120
;; expected: 120

(define (n_choose_k (n : Number) (k : Number))
  (bind n 10)
  (bind k 3)
  (n_choose_k)
)
