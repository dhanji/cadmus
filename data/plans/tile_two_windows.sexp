;; Tile two application windows side by side via peekaboo
(define (tile-two-windows (left_app : String) (right_app : String))
  (bind left_app "Terminal")
  (bind right_app "Safari")
  (shell_peekaboo_window_move :args "--app $left_app --x 0 --y 25")
  (shell_peekaboo_window_resize :args "--app $left_app --width 960 --height 1050")
  (shell_peekaboo_window_move :args "--app $right_app --x 960 --y 25")
  (shell_peekaboo_window_resize :args "--app $right_app --width 960 --height 1050"))
