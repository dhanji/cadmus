;; Find recent PDF: find PDFs downloaded in the last 24 hours
(define (find-recent-pdf (path : Dir))
  (find_recent :dir "$path" :days "1")
  (find_matching :pattern "*.pdf"))