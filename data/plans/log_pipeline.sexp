;; Log pipeline: awk extract, sed transform, head, tail
(define (log-pipeline (logfile : File))
  (awk_extract :program "{print $1, $4, $5}")
  (sed_script :script "s/ERROR/[ERR]/g; s/WARN/[WRN]/g")
  (head :count "100")
  (tail :count "50"))
