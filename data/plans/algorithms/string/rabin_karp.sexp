;; Rabin-Karp string search using rolling hash
;; Example: search "ABAB" in "ABABDABACDABABCABAB" = 0
;; expected: 0

(define (rabin_karp (text : String) (pattern : String))
  (bind pattern ""ABAB"")
  (bind text ""ABABDABACDABABCABAB"")
  (rabin_karp)
)
