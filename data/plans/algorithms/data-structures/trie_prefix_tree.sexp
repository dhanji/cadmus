;; Trie: insert words and count those with prefix "ap"
;; Words: apple, app, ape, bat, bar → 3 words with prefix "ap"
;; expected: 3

(define (trie_prefix_tree (n : Number))
  (bind n 0)
  (trie_prefix_tree)
)
