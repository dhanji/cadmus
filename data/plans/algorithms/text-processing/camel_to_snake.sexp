;; Camel to snake case: convert camelCase to snake_case
;; Example: camel_to_snake("helloWorldFoo") = "hello_world_foo"
;; expected: hello_world_foo

(define (camel_to_snake (s : String))
  (bind s "helloWorldFoo")
  (camel_to_snake)
)
