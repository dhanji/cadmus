;; Longest increasing subsequence length (O(n²) DP)
;; expected: 4

(define (longest_increasing_subsequence (lst : (List Number))) : Number
  (bind lst (list 10 9 2 5 3 7 101 18))
  (let ([n (length lst)]
        [dp (make n 1)])
    (for/each ([i (range 1 n)])
      (for/each ([j (range 0 i)])
        (when (< (ref lst j) (ref lst i))
          (let ([cur (ref dp i)]
                [prev (ref dp j)])
            (set! dp i (max cur (+ prev 1)))))))
    (for/fold ([mx 0]) ([i (range 0 n)])
      (max mx (ref dp i)))))
