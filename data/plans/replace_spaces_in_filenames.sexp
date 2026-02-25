;; Replace spaces in filenames: batch rename files replacing spaces with dashes
(define (replace-spaces-in-filenames (path : Dir))
  (replace_in_filenames :find " " :replace "-"))