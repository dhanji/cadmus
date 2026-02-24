;; Queue operations: simulate a FIFO queue
;; Example: enqueue 1,2,3, dequeue, dequeue → (1 2)
;; expected: (1 2)

(define (queue_operations (ops : (List Number)))
  (bind ops (list 1 2 3 0 0))
  (queue_operations)
)
