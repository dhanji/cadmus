;; Zip folder: create a zip archive of the current directory
(define (zip-folder (path : Dir))
  (pack_zip :output "archive.zip"))