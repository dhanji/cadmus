;; Huffman coding: build tree and compute total encoded bits
;; Example: "aabbbcccc" frequencies: a=2 b=3 c=4 → total bits = 15
;; expected: 18

(define (huffman_coding (s : String))
  (bind s "aabbbcccc")
  (huffman_coding)
)
