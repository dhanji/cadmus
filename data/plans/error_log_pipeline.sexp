;; Error log pipeline: read log file, match error lines, format as html code block
(define (error-log-pipeline (logfile : (File Text)))
  (read_file)
  (format_html_code_block))
