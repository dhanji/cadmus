;; Snake to title case: convert snake_case to Title Case
;; Example: snake_to_title("hello_world_foo") = "Hello World Foo"
;; expected: Hello World Foo

(define (snake_to_title (s : String))
  (bind s "hello_world_foo")
  (string_split :s $s :delim "_")
  (string_join :lst $step-1 :sep " ")
  (title_case :s $step-2)
)
