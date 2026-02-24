;; Radix sort (LSD, base 10)
;; Example: sort (170 45 75 90 802 24 2 66) = (2 24 45 66 75 90 170 802)
;; expected: (2 24 45 66 75 90 170 802)

(define (radix_sort (lst : (List Number)))
  (bind lst (list 170 45 75 90 802 24 2 66))
  (radix_sort)
)
