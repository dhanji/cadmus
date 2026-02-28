;; Spin up a web server on localhost port 8080 serving hello world
(define (spin-up-web-server)
  (http_server :port "8080"))
