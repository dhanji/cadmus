;; Tower of Hanoi: count moves to solve n-disk puzzle = 2^n - 1
;; Example: hanoi(4) = 15
;; expected: 15

(define (tower_of_hanoi (n : Number))
  (bind n 4)
  (expt :x 2 :y $n)
  (subtract :x $step-1 :y 1)
)
