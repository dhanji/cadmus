;; Clean desktop: move files on the Desktop into sensible folders, leaving shortcuts alone
(define (clean-desktop)
  (organize_by_extension :path "~/Desktop"))