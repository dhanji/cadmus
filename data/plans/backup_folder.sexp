;; Create a timestamped backup copy of a folder
(define (backup-folder (path : Dir) (dest : Path))
  (backup_timestamped :dest "$dest"))