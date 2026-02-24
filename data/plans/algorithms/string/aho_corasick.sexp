;; Aho-Corasick: multi-pattern search (simplified — count total matches)
;; Example: count "he","she","his","hers" in "ahishers" = 4
;; expected: 4

(define (aho_corasick (text : String))
  (bind text "ahishers")
  (aho_corasick)
)
