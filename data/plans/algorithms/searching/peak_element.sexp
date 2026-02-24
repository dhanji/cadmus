;; Peak element: find local maximum in array
;; Example: (1 2 3 1) → peak at index 2
;; expected: 2

(define (peak_element (nums : (List Number)))
  (bind nums (list 1 2 3 1))
  (peak_element)
)
