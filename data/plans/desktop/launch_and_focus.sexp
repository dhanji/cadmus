;; Launch an application and focus its window via peekaboo
(define (launch-and-focus (app : String))
  (bind app "Calculator")
  (shell_peekaboo_app_launch :args "$app")
  (shell_peekaboo_window_focus :args "--app $app"))
