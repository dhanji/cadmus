;; Add route to web server with pipeline: read file, match errors, return as html code block
(define (add-route-web-server (route : String) (handler_program : String))
  (add_route :path "$route" :handler_program "$handler_program"))
