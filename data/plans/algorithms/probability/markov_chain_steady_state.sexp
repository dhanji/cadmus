;; Markov chain steady state: find stationary distribution by power iteration
;; Example: 2-state chain with P=[[0.7,0.3],[0.4,0.6]] → (0.5714 0.4286)
;; expected: (0.5714 0.4286)

(define (markov_chain_steady_state (n : Number))
  (bind n 2)
  (markov_chain_steady_state)
)
