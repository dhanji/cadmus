;; Tarjan's SCC: find strongly connected components
;; Graph: 0→1  1→2  2→0  2→3  3→4  4→3
;; SCCs: {0,1,2} {3,4}
;; expected: ((0 1 2) (3 4))

(define (tarjan_scc (n : Number))
  (bind n 5)
  (tarjan_scc)
)
