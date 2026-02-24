;; Download and extract: extract archive, sort contents
(define (download-and-extract (archive : (File (Archive Bytes Zip))))
  (extract_archive)
  (sort_by "name"))
