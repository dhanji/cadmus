;; Capture UI elements and click a target element via peekaboo
(define (see-and-click (app : String) (element : String))
  (bind app "Calculator")
  (bind element "B1")
  (shell_peekaboo_see :args "--app $app --json")
  (shell_peekaboo_click :args "--on $element --app $app"))
