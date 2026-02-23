;; Factorial: n! = n × (n-1) × ... × 1
;; expected: 3628800

(define (factorial (n : Number)) : Number
  (bind n 10)
  (for/fold ([acc 1]) ([i (range 1 (+ n 1))])
    (* acc i)))
