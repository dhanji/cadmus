;; Rod cutting: maximum revenue from cutting rod of length n
;; Prices: length 1=1, 2=5, 3=8, 4=9, 5=10, 6=17, 7=17, 8=20
;; Example: rod length 8 = 22 (cut into 2+6)
;; expected: 22

(define (rod_cutting (n : Number))
  (bind n 8)
  (rod_cutting)
)
