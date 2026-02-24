;; Repack comic archives: extract all CBZ files and combine into one
(define (repack-comics (path : Dir))
  (list_dir)
  (find_matching :pattern "*.cbz")
  (sort_by "name")
  (extract_archive :each)
  (pack_archive :output "combined.cbz"))
