;; Max stack: stack with maximum query support
;; Example: push 3, push 5, max query, pop, max query → (5 3)
;; expected: (5 3)

(define (max_stack (ops : (List Number)))
  (bind ops (list 3 5 -1 0 -1))
  (max_stack)
)
