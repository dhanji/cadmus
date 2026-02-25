;; Click a field, type text, and press return via peekaboo
(define (click-type-submit (app : String) (element : String) (text : String))
  (bind app "Safari")
  (bind element "T1")
  (bind text "hello world")
  (shell_peekaboo_click :args "--on $element --app $app")
  (shell_peekaboo_type :args "$text --app $app")
  (shell_peekaboo_press :args "return"))
