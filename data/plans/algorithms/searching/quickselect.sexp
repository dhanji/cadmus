;; Quickselect: find k-th smallest element (0-indexed)
;; Example: 3rd smallest in (3 6 2 8 1 5 7 4) = 4
;; expected: 4

(define (quickselect (lst : (List Number)) (k : Number))
  (bind k 3)
  (bind lst (list 3 6 2 8 1 5 7 4))
  (quickselect)
)
