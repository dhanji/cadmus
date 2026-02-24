;; Morse code encode: convert text to morse code
;; Example: "SOS" = "... --- ..."
;; expected: ... --- ...

(define (morse_code_encode (s : String))
  (bind s "SOS")
  (morse_code_encode)
)
