;; LRU cache: simulate cache with capacity 2
;; Access: 1 2 3 1 → after 3, evict 1; after 1, evict 2
;; Final cache contains: (3 1)
;; expected: (1 3)

(define (lru_cache_eviction (n : Number))
  (bind n 0)
  (lru_cache_eviction)
)
