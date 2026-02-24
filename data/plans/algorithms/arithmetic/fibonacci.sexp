;; Fibonacci: F(n) where F(0)=0, F(1)=1, F(n)=F(n-1)+F(n-2)
;; Example: F(10) = 55
;; expected: 55

(define (fibonacci (n : Number))
  (bind n 10)
  (fibonacci)
)
