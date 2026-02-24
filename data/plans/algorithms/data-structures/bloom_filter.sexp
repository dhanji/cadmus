;; Bloom filter: probabilistic set membership (simplified)
;; Insert "hello" "world", test "hello" "foo" → (#t #f)
;; expected: (#t #f)

(define (bloom_filter (n : Number))
  (bind n 0)
  (bloom_filter)
)
