;; Stack operations: simulate a LIFO stack
;; Example: push 3, push 5, pop, push 7 → (3 7)
;; expected: (3 7)

(define (stack_operations (ops : (List Number)))
  (bind ops (list 3 5 0 7))
  (stack_operations)
)
