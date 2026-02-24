;; Download and inspect: extract, find PNGs, sort
(define (download-inspect (file : (File (Archive Bytes Zip))))
  (extract_archive)
  (find_matching :pattern "*.png")
  (sort_by "name"))
