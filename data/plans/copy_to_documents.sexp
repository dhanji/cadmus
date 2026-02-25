;; Copy to documents: recursively copy the current directory to ~/Documents
(define (copy-to-documents (path : Dir))
  (sync :dest "~/Documents"))