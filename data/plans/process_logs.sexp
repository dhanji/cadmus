;; Process logs: extract fields with awk, transform with sed
(define (process-logs (logfile : File) (sed_pattern : Pattern))
  (awk_extract :program "{print $1, $3, $5}")
  (sed_script :script "$sed_pattern"))
