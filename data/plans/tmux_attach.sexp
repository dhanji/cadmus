;; Attach to an existing tmux session or create a new one
(define (tmux-attach (session_name : Name))
  (tmux_attach :name "$session_name"))