;; Boyer-Moore majority vote: find element appearing > n/2 times
;; Example: majority in (3 3 4 2 3 3 3) = 3
;; expected: 3

(define (boyer_moore_majority_vote (lst : (List Number)))
  (bind lst (list 3 3 4 2 3 3 3))
  (boyer_moore_majority_vote)
)
