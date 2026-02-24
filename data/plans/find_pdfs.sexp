;; Find PDFs in a directory
(define (find-pdfs (path : Dir) (keyword : Pattern))
  (list_dir)
  (find_matching :pattern "*.pdf")
  (sort_by "name"))
