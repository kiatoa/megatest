(use general-lib)
(use typed-records)
(use regex-literals)
(use regex)
(use sql-de-lite)
(use posix)
(use files)
(use s11n)
(use ports)
(use z3)
(use base64)
(use matchable)




(define (cli-arg arg #!key (default #f) (is-list #f))
  (let* ((temp    (skim-cmdline-opts-withargs-by-regex arg)))
      (if (> (length temp) 0)
          (if is-list
              temp
              (car temp))
          default)))

(define (cli-switch arg)
  (let ((temp (skim-cmdline-opts-noarg-by-regex arg)))
    (if (> (length temp) 0)
        (car temp)
        #f)))


         
(defstruct nbjob
    pool
    jobid
    owner
    mtver
    user
    status
    cmdline
    execute)

(define (cmdline->execute cmdline)
  (let* ((match (string-match ".*-execute\\s+(\\S+)" cmdline)))
    (if match
        (with-input-from-string (z3:decode-buffer (base64-decode (cadr match))) read)
        #f)))


(define (nbjob-execute-ref nbjob key #!key (default #f))
  (let ((execute (nbjob-execute nbjob)))
    (if (list? execute)
        (let* ((match (alist-ref key execute)))
          (if match
              (if (list? match) (car match) match)
              default))
        default)))

(define (nbjob-process pool nbstatus-line)
  (let ((toks (string-split nbstatus-line ",")))
    (if (eq? 4 (length toks))
        (if (equal? (list-ref toks 1) "Jobid")
            #f
            (begin
              (let ((res
                     (make-nbjob
                      pool:    pool
                      status:  (list-ref toks 0)
                      jobid:   (list-ref toks 1)
                      user:    (list-ref toks 2)
                      cmdline: (list-ref toks 3)
                      execute: (cmdline->execute (list-ref toks 3))
                      )))
                res)))
        #f)))

(define (get-mtest-nb-jobs user nbpools #!key (cmdline-filter "megatest"))
  (let* ((res
          (apply append
                 (map (lambda (pool)
                        (let* (;;(user-filter ".*")
                               (user-filter user)
                               (cmd
                                (conc "nbstatus jobs --tar "pool" --fields status,jobid,user,cmdline --format csv "
                                      "'USER=~\""user-filter
                                      "\"&&cmdline=~\""cmdline-filter"\"'"))
                               (res (do-or-die cmd)))
                          (filter nbjob?
                                  (map (lambda (line)
                                         (nbjob-process pool line))
                                       (string-split res "\n")))))
                      nbpools))))
    res))

;;(define foo (get-mtest-nb-jobs "bjbarcla" '("pdx_normal" "pdx_critical")))

(define (cmdline->execute cmdline)
  (let* ((match (string-match ".*-execute\\s+(\\S+)" cmdline)))
    (if match
        (with-input-from-string (z3:decode-buffer (base64-decode (cadr match))) read)
        #f)))

;;;; kill jobs in netbatch for this area
(define (kill-mtest-jobs-in-netbatch)
  (let ((pwd (get-environment-variable "PWD"))
        (jobs (get-mtest-nb-jobs (get-environment-variable "USER") '("pdx_normal" "pdx_critical") )))

    (for-each
     (lambda (job)
       (let* ((jobid  (nbjob-jobid job))
              (pool   (nbjob-pool job))
              (status (nbjob-status job))
              (cmd (conc "nbjob --target "pool" remove "jobid)))
         ;;(print status)
         (print  cmd)
         (system cmd)))
                                        ;(pp (nbjob->alist job))
     (filter
      (lambda (job)
        (equal? (nbjob-execute-ref job 'toppath) pwd))
      jobs))
    (length jobs)

    ))


;;;; kill megatest jobs in running in netbatch
(define (kill-in-db #!key (megatest_exe "megatest"))
  (let* ((all-targ-patt (do-or-die "sqlite3 megatest.db \"select id from keys\" | tr \"\\n1234567890\" \"/%%%%%%%%%%\" | sed 's/\\/$//'"))
         )
    (for-each (lambda (state)
                (let* ((cmd (conc megatest_exe " -state "state" -set-state-status KILLED,n/a -testpatt % -target "all-targ-patt" -runname %")))
                  (print cmd)
                  (system cmd)))
              '("REMOTEHOSTSTART" "LAUNCHED" "RUNNING" "KEEP_TRYING" "PREQ_FAIL"))))



;;;; kill megatests and dashboards in this area running on this host
(define (kill-mtest-dboard)
  
  (let* ((this-toppath (pid->cwd (current-process-id)))
         (tmppath      (toppath->tmppath this-toppath))
         (config       (let ((res (conc this-toppath "/megatest.config")))
                         (when (not (file-exists? res))
                           (ierr "This is not a megatest run area; "res" does not exist.  Aborting.")
                           (exit 2))
                         res))
         (mtest-procs (get-my-mtest-procs))
         (dashboard-procs (get-my-dashboard-procs))
         (all-pids (map proc-PID (append mtest-procs dashboard-procs)))
         (our-pids (filter (lambda (pid)
                             ;;(print (pid-COMMAND pid))
                             (and
                              (equal? (pid->cwd pid) this-toppath)
                              
                              ))
                           all-pids)))

    (if (null? our-pids)
        (inote "No mtest or dboard processes on this host in in this runarea.")
        (begin
          (iwarn "Killing all megatest and dashboard processes on this host.")
          (gracefully-kill-pids our-pids)))
    ))



(define (db-mt-version dbpath)
  (let* ((cmd (conc "sqlite3 "dbpath" 'select val from metadat where var=\"MEGATEST_VERSION\"'"))
         (res (do-or-die cmd)))
    res))
   
;; TODO
(define (db-islocked? dbpath)
  (let-values (((ec so se) (isys (conc "sqlite3 "dbpath" vacuum"))))
    (let* ((message se)
           (is-locked (string-match "^.*database is locked.*$" message)))
      (inote "dbfile - "dbpath "; message - "message)
      is-locked)))

(define (db-unlock dbpath)
  (system (conc "/nfs/site/bjbarcla/bin/unlock_db.sh " dbpath))

  ;; (let* ((temp-dbpath (conc "/tmp/"(get-environment-variable "USER")"-"(current-process-id)".db")))
  ;;   (inote "Unlocking "dbpath)
  ;;   (do-or-die (conc "cp "dbpath" "temp-dbpath))
  ;;   (do-or-die (conc "rm -f "dbpath))
  ;;   (let* ((cmd (conc "sqlite3 "temp-dbpath" .dump | sqlite3 "dbpath)))
  ;;     (inote "Running: "cmd)
  ;;     (system cmd))
  ;;   ;;(do-or-die "sqlite3 "temp-dbpath" .dump | sqlite3 "dbpath)
  ;;   (if (db-islocked? dbpath)
  ;;       (begin
  ;;         (ierr "Could not unlock "dbpath)
  ;;         (exit 5))
  ;;       (inote "Unlocked "dbpath))
  ;;   #t)

  )


(define *user* (do-or-die "ls -ld . | awk '{print $3}'"))

(define (false-on-exception thunk)
  (handle-exceptions exn #f (thunk) ))

(define (safe-file-exists? path-string)
  (false-on-exception (lambda () (file-exists? path-string))))

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

(define (toppath->tmppath toppath)
  (let* ((user *user*)
         (area (car (string-split
                     (car (reverse (string-split toppath "/")))
                     ".")))
         (dotified-path (string-substitute "/" "." toppath "all")))
    (conc "/tmp/" user "/megatest_localdb/" area "/" dotified-path)))


(define (delete-db dbfile)
  (let* ((db-files (glob (conc dbfile "*"))))
    (for-each
     (lambda (file)
       (inote "delete file " file)
       (if (delete-file* file)
           (inote "Removed file - " file)
           (iwarn "Could Not Remove file - " file))
       )
     db-files)))

(define (check-db dbfile)
  (let* ((has-wal (safe-file-exists? (conc dbfile "-wal")))
         (has-shm (safe-file-exists? (conc dbfile "-shm")))
         (has-journal (safe-file-exists? (conc dbfile "-journal")))
         (has-db (safe-file-exists? dbfile))
         (ok-flag #t))
    (when has-journal
      (iwarn "Journal exists - "(conc dbfile "-journal"))
      )
    (when has-wal
      (set! ok-flag #f)
      (iwarn "Wal-mode db exists: "(conc dbfile "-wal")))
    (if (not has-db)
        (begin
          (inote "db does not exists " dbfile)
          (set! ok-flag #f))
        (let* ((db-size (file-size dbfile)))
          (inote "db size = " db-size " -- " dbfile)
          (when (member db-size (list 0 1024))
            (iwarn "db has bad size - "db-size" -- "dbfile)
            (set! ok-flag #f))))
    ok-flag))
               

(define (linux-get-process-info-records)
  (let* ((raw (do-or-die "/bin/ps auwx"))
         (all-lines (string-split raw "\n"))
         (lines (cdr all-lines)) ;; skip title lines
         (re (regexp "^(\\S+)\\s+(\\d+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(.*)$")))
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
             (equal? *user* (proc-USER a-proc))
             (string-match "^.*/mtest\\s+.*-server.*" (proc-COMMAND a-proc))))
          procs)))
    my-mtest-procs))

(define (get-my-mtest-procs)
  (let* ((procs (linux-get-process-info-records))
        (my-mtest-procs
         (filter
          (lambda (a-proc)
            (and
             (equal? *user* (proc-USER a-proc))
             (string-match "^.*/m(ega)?test .*" (proc-COMMAND a-proc))
             (not (string-match "^.*/mtest-repair.*" (proc-COMMAND a-proc)))))
          procs)))
    my-mtest-procs))

(define (get-my-dashboard-procs)
  (let* ((procs (linux-get-process-info-records))
        (my-dboard-procs
         (filter
          (lambda (a-proc)
            (and
             (equal? *user* (proc-USER a-proc))
             (string-match "^.*/dboard.*" (proc-COMMAND a-proc))))
          procs)))
    my-dboard-procs))


(define (pid->environ-hash pid)
  (let* ((envfile (conc "/proc/"pid"/environ"))
         (ht (make-hash-table))
         (rawdata (with-input-from-file envfile read-string))
         (lines (string-split rawdata  (make-string 1 #\nul ))))
    (for-each
     (lambda (line)
       (let ((match (string-match "(^[^=]+)=(.*)" line)))
         (if match
             (hash-table-set! ht (list-ref match 1) (list-ref match 2)))))
     lines)
    ht))

(define (pid->cwd pid)
  (read-symbolic-link (conc "/proc/"pid"/cwd")))

(define (pid->mtest-monitor-db-file pid #!key (megatest_exe "megatest"))
  (let* ((env   (pid->environ-hash pid))
         (ltdir (hash-table-ref/default env "MT_LINKTREE" #f))
         (radir (hash-table-ref/default env "MT_RUN_AREA_HOME" #f))
         (cwd   (pid->cwd pid)))
    (let ((res
           (cond
            (ltdir (conc ltdir "/.db/monitor.db"))
            (radir (conc
                    (do-or-die
                     (conc megatest_exe " -start-dir "radir" -show-config -section setup -var linktree"))
                    "/.db/monitor.db"))
            (cwd  (conc
                   (do-or-die
                    (conc megatest_exe " -start-dir "cwd" -show-config -section setup -var linktree"))
                   "/.db/monitor.db"))
            
            (else #f))))
      res)))
      




(define (get-mdb-status mdb-file pid)
    ;; select state from servers where pid='4465';
  
  (cond
   ((not (string? mdb-file)) (conc "mdb-file could not be determined for pid " pid ">>"mdb-file ))
   ((not (safe-file-exists? mdb-file)) (conc "mdb-file does not exist for pid "pid" : "mdb-file))
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


(define (gracefully-kill-pids pids)
  (for-each (lambda (pid)
              (print "kill "pid)
              (system (conc "kill "pid)))
            pids)
  (sleep 5)
  (let* ((procs-left (linux-get-process-info-records))
         (pids-left (map proc-PID procs-left)))
    (for-each (lambda (pid)
                (when (member pid pids-left)
                  (print "kill -9"pid)
                  (system (conc "kill -9 "pid))))
              pids)))
  
            
              
(define (kill pid)
  (print "KILL "pid)
  (do-or-die (conc "kill -9 "pid)))

(define (reap-defunct-mtest-server-pid pid)
  (let ((status (mtest-server-pid->status pid)))
    (print pid"->"(mtest-server-pid->status pid))
    (if (member status (list "running" "dbprep" "available" "collision"))
        (print "pid="pid" in status "status" -- not killing")
        (kill pid))))
        
;; (let* ((procs (get-my-mtest-server-procs))
;;        (pids (map proc-PID procs))
;;        )
  
;;   (for-each reap-defunct-mtest-server-pid pids))



(define (mtdbver->mtrelver mtdbver)
  (let* ((table-alist '(
                        ("1.5402" . "1.54/02")
                        ("1.5406" . "1.54/05")
                        ("1.5408" . "1.54/07")
                        ("1.5409" . "1.54/09")
                        ("1.5412" . "1.54/12")
                        ("1.5413" . "1.54/13")
                        ("1.5414" . "1.54/14")
                        ("1.5415" . "1.54/15")
                        ("1.5416" . "1.54/16")
                        ("1.5417" . "1.54/17")
                        ("1.5418" . "1.54/18")
                        ("1.5419" . "1.54/19")
                        ("1.5421" . "1.54/21")
                        ("1.5411" . "1.54/support-for-skip")
                        ("1.5522" . "1.55/22")
                        ("1.5523" . "1.55/23")
                        ("1.5524" . "1.55/24")
                        ("1.5525" . "1.55/25")
                        ("1.6001" . "1.60/01")
                        ("1.6002" . "1.60/02")
                        ("1.6003" . "1.60/03")
                        ("1.6004" . "1.60/04")
                        ("1.6005" . "1.60/05")
                        ("1.6006" . "1.60/06")
                        ("1.6007" . "1.60/07")
                        ("1.6008" . "1.60/08")
                        ("1.6009" . "1.60/09")
                        ("1.6009" . "1.60/11")
                        ("1.6012" . "1.60/12")
                        ("1.6013" . "1.60/13")
                        ("1.6014" . "1.60/14")
                        ("1.6015" . "1.60/15")
                        ("1.6016" . "1.60/16")
                        ("1.6017" . "1.60/17")
                        ("1.6018" . "1.60/18")
                        ("1.6019" . "1.60/19")
                        ("1.6021" . "1.60/21")
                        ("1.6022" . "1.60/22")
                        ("1.6023" . "1.60/23")
                        ("1.6024" . "1.60/24")
                        ("1.6025" . "1.60/25")
                        ("1.6026" . "1.60/26")
                        ("1.6027" . "1.60/27")
                        ("1.6028" . "1.60/28")
                        ;;("1.6029" . "1.60/29")
                        ("1.6029" . "1.60/29a")
                        ("1.6031" . "1.60/31")
                        ("1.6101" . "1.61/01")
                        ("1.6101" . "1.61/01a")
                        ("1.6102-c2ba" . "1.61/02")
                        ("1.6103-3e88" . "1.61/03")
                        ("1.6104-ee53" . "1.61/04")
                        ("1.6105-232b" . "1.61/05")
                        ("1.6201-e652" . "1.62/01")
                        ("1.6204-c74d" . "1.62/04")
                        ("1.6205-aff0" . "1.62/05")
                        ("1.6207-6f59" . "1.62/07")
                        ("1.6301-fbf0" . "1.63/01")
                        ("1.6302-da4a" . "1.63/02")
                        ("1.6303-aa5f" . "1.63/03")
                        ("1.6304-fa49" . "1.63/04")
                        ("1.6305-a03b" . "1.63/05")
                        ("1.6306-7a12" . "1.63/06")
                        ("1.6307-fb5d" . "1.63/07")
                        ("1.6308-35e0" . "1.63/08")
                        ("1.6309-738c" . "1.63/09")
                        ("1.6309-880c" . "1.63/09a")
                        ("1.6309-b566" . "1.63/09b")
                        ("1.6311-fb43" . "1.63/11")
                        ("1.6311-fb43" . "1.63/11b")
                        ("1.6311-8a6c" . "1.63/11b")
                        ("1.6402-03c5" . "1.64/02")
                        )
                      )
         (res (alist-ref mtdbver table-alist equal?)))
    res))

