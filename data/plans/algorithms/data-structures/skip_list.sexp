;; Skip list: insert elements and check membership
;; Insert 3 6 7 9 12, check 7 and 5
;; expected: (#t #f)

(define (skip_list (n : Number))
  (bind n 0)
  (skip_list)
)
