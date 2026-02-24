;; Euler path check: does graph have an Eulerian path?
;; Graph: 0-1, 0-2, 1-2, 2-3 (vertices 0,1 have odd degree → Euler path exists)
;; expected: #t

(define (has_euler_path (n : Number))
  (bind n 4)
  (has_euler_path)
)
