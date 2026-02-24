;; Extract a CBZ archive
(define (extract-cbz (path : (File (Archive (File Image) Cbz))))
  (extract_archive))
