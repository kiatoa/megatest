;;(use general-lib)
;;(import scheme chicken extras ports data-structures)
;;(use directory-utils filepath)
(use typed-records)
;;(use regex-literals)
;;(use regex)

(import scheme chicken extras ports data-structures )
(use posix regex ansi-escape-sequences test srfi-1 irregex slice srfi-13 rfc3339 scsh-process directory-utils uuid-lib filepath srfi-19 posix-extras hostinfo)
(use pathname-expand)
(use sql-de-lite)

(define (safe-file-exists? path-string)
  (false-on-exception (lambda () (file-exists? path-string))))

;; assumes this compiled binary lives in <isoenv core root>/bin
(define (find-dir-on-path-holding-binary binary)
  (let ((filter-result
         (filter (lambda (dir)
                   (safe-file-exists? (conc dir "/" binary)))
                 (string-split (get-environment-variable "PATH") ":"))))
    (if (> (length filter-result) 0)
        (car filter-result)
        (abort (conc "Error: Cannot resolve path to binary '"binary"'. ")))))

(define (get-dir-holding-self)
  (let ((binname (car (argv))))
    (cond
     ((safe-file-exists?  binname) (pathname-expand binname))
     (else  (find-dir-on-path-holding-binary binname)))))

(define (self-exe-fullpath)
  (let ((binname (car (argv))))
    (cond
     ((safe-file-exists?  binname) (pathname-expand binname))
     (else  (conc (find-dir-on-path-holding-binary binname) "/" binname)))))

(let* ((lib-path (conc (get-dir-holding-self) "/lib/ducttape-lib.so")))
  (print "loading "lib-path)
  (load lib-path))
(import ducttape-lib)



(define (get-my-mtest-server-procs)
  (let* ((procs (linux-get-process-info-records))
        (my-mtest-procs
         (filter
          (lambda (a-proc)
            (and
             (equal? (get-environment-variable "USER") (proc-USER a-proc))
             (string-match (regexp "^.*\\/mtest\\s+.*-server.*") (proc-COMMAND a-proc))))
          procs)))
    my-mtest-procs))



(define (pid->mtest-monitor-db-file pid)
  (let* ((env   (pid->environ-hash pid))
         (ltdir (hash-table-ref/default env "MT_LINKTREE" #f))
         (radir (hash-table-ref/default env "MT_RUN_AREA_HOME" #f))
         (cwd   (pid->cwd pid)))
    (let ((res
           (cond
            (ltdir (conc ltdir "/.db/monitor.db"))
            (radir (conc
                    (do-or-die
                     (conc "megatest -start-dir "radir" -show-config -section setup -var linktree"))
                    "/.db/monitor.db"))
            (cwd  (conc
                   (do-or-die
                    (conc "megatest -start-dir "cwd" -show-config -section setup -var linktree"))
                   "/.db/monitor.db"))
            
            (else #f))))
      res)))
      
(define (get-mdb-status mdb-file pid)
    ;; select state from servers where pid='4465';
  
  (cond
   ((not (string? mdb-file)) (conc "mdb-file could not be determined for pid " pid ">>"mdb-file ))
   ((not (file-exists? mdb-file)) (conc "mdb-file does not exist for pid "pid" : "mdb-file))
   (else
    (let ((dbh (open-database mdb-file)))
      
      (set-busy-handler! dbh 10000)
      (let* ((sql-str "select state from servers where pid=?;")
             (stm (sql dbh sql-str))
             (alists (query fetch-alists stm (->string pid))))
        (if (null? alists)
            "server pid not in monitor.db"
            (cdr (car (car alists)))))))))

    
(define (mtest-server-pid->status pid)
  (let* ((mdb-file (pid->mtest-monitor-db-file pid)))
    (if mdb-file
        (get-mdb-status mdb-file pid)
        "no monitor.db file could be found"
        )))


(define (kill pid)
  (print "KILL "pid)
  (do-or-die (conc "kill -9 "pid)))

(define (reap-defunct-mtest-server-pid pid)
  (let ((status (mtest-server-pid->status pid)))
    (print pid"->"(mtest-server-pid->status pid))
    (if (member status (list "running" "dbprep" "available" "collision"))
        (print "pid="pid" in status "status" -- not killing")
        (kill pid))))

(define (make-it-so)
  (let* ((procs (get-my-mtest-server-procs))
         (pids (map proc-PID procs)))
    (for-each reap-defunct-mtest-server-pid pids)))


(define (run-self-on-remote-host) #t)

(if (file-exists? ".homehost")
    (let* ((homehost (with-input-from-file ".homehost" (lambda () (read)) ))
           (thishost (current-ip-string))
          (me-exe   (self-exe-fullpath))
          (sshcmd      (conc "ssh "homehost" 'cd "(get-environment-variable "PWD")" && "me-exe"'")))
      (print ">>"homehost"<<")
      (print ">>"me-exe"<<")
      (print ">>"thishost"<<")
      (print ">>"sshcmd"<<")
      (if (equal? thishost homehost)
          (print "makeitso")
          (do-or-die sshcmd))
      
      ))
