;; List all running applications via peekaboo
(define (list-running-apps (dummy : String))
  (bind dummy ".")
  (shell_peekaboo_list_apps))
