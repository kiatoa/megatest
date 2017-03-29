#!/p/foundry/env/pkgs/chicken/4.10.0_ext/bin/csi -s

(use general-lib)
(use typed-records)
(use regex-literals)
(use regex)
(use sql-de-lite)

(defstruct proc
    (USER "")
  (PID -1)
  (%CPU -1.0)
  (%MEM -1.0)
  (VSZ -1)
  (RSS -1)
  (TTY "")
  (STAT "")
  (START "")
  (TIME "")
  (COMMAND ""))

(define (linux-get-process-info-records)
  (let* ((raw (do-or-die "/bin/ps auwx"))
         (all-lines (string-split raw "\n"))
         (lines (cdr all-lines)) ;; skip title lines
         (re #/^(\S+)\s+(\d+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(.*)$/))
    (filter
     proc?
     (map
      (lambda (line)
        (let ((match (string-match re line)))
          (if match
              (make-proc
               USER:    (list-ref match 1)
               PID:     (string->number (list-ref match 2))
               %CPU:    (string->number (list-ref match 3))
               %MEM:    (string->number (list-ref match 4))
               VSZ:     (string->number (list-ref match 5))
               RSS:     (string->number (list-ref match 6))
               TTY:     (string->number (list-ref match 7))
               STAT:    (list-ref match 8)
               START:   (list-ref match 9)
               TIME:    (list-ref match 10)
               COMMAND: (list-ref match 11))
              #f)))
      lines))))
        
(define (get-my-mtest-server-procs)
  (let* ((procs (linux-get-process-info-records))
        (my-mtest-procs
         (filter
          (lambda (a-proc)
            (and
             (equal? (get-environment-variable "USER") (proc-USER a-proc))
             (string-match #/^.*\/mtest\s+.*-server.*/ (proc-COMMAND a-proc))))
          procs)))
    my-mtest-procs))


(define (pid->environ-hash pid)
  (let* ((envfile (conc "/proc/"pid"/environ"))
         (ht (make-hash-table))
         (rawdata (with-input-from-file envfile read-string))
         (lines (string-split rawdata  (make-string 1 #\nul ))))
    (for-each
     (lambda (line)
       (let ((match (string-match #/(^[^=]+)=(.*)/ line)))
         (if match
             (hash-table-set! ht (list-ref match 1) (list-ref match 2)))))
     lines)
    ht))

(define (pid->cwd pid)
  (read-symbolic-link (conc "/proc/"pid"/cwd")))

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
        
(let* ((procs (get-my-mtest-server-procs))
       (pids (map proc-PID procs))
       )
  
  (for-each reap-defunct-mtest-server-pid pids))

