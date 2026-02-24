;; Maximum subarray product: contiguous subarray with largest product
;; Example: (2 3 -2 4) → max product 6
;; expected: 6

(define (max_subarray_product (nums : (List Number)))
  (bind nums (list 2 3 -2 4))
  (max_subarray_product)
)
