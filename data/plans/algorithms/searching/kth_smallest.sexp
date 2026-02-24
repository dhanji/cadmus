;; Kth smallest: find the kth smallest element
;; Example: 3rd smallest of (7 10 4 3 20 15) = 7
;; expected: 7

(define (kth_smallest (nums : (List Number)) (k : Number))
  (bind k 3)
  (bind nums (list 7 10 4 3 20 15))
  (kth_smallest)
)
