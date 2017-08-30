#!/p/foundry/env/pkgs/chicken/4.10.0_ext/bin/csi -s

(use chicken)
(use data-structures)


(include "/nfs/site/home/bjbarcla/bin2/mtest-repair-lib.scm")
(glib-color-mode 1)

;;; check mtver in xterm
(let ((mt-ver (do-or-die "megatest -version")))
  (when (member mt-ver '("1.6309-738c" "1.6029"))
      (iwarn "This xterm has an older version of megatest.")
      (ierr "Please load latest megatest version to proceed.")
      (print "eg.: source ../scripts/newrel-setup.csh 1.63/11b")
      (exit 3)))


;;;; kill netbatch jobs from this megatest
;; TODO!


(define *diag* #t)
;;(define *user* (get-environment-variable "USER"))
(define *user* (do-or-die "ls -ld . | awk '{print $3}'"))
(print "user="*user*)
;;;; delete .homehost .homehost.config
;;;; if not on homehost, ssh homehost, cd here, killall mtest dboard
(if (not *diag*)
    (when (file-exists? ".homehost.config")
      (delete-db ".homehost.config")))
    
(when (file-exists? ".homehost")
  (let* ((homehost (with-input-from-file ".homehost" (lambda () (read)))))
    (let* ((homehostname (do-or-die "host `cat .homehost` | sed 's/.$//' | awk '{print $NF}' | awk -F. '{print $1}'"))
           (thishostname (get-environment-variable "HOST")))
      (when (not (equal? homehostname thishostname))
        (let* ((this-exe-compiled (car (argv)))
               (this-exe "/nfs/site/home/bjbarcla/bin2/mtest-diag.scm")
               (cmd (conc "ssh "homehostname" 'cd "(get-environment-variable "PWD")" && "this-exe"'")))
          (iwarn "Running on the homehost -- "homehostname)
          ;;(iwarn "eg: % ssh "homehostname" 'cd "(get-environment-variable "PWD")" && "(car (argv))"'")
          (print "cmd="cmd)
          ;;(inote "sleeping for 5 seconds.  hit ctrl-c now to run on homehost or wait to proceed.")
          (system cmd)
          (exit 0))))))




;;;; kill megatests and dashboards in this area
(define (kill-mtest-dboard)
  (if *diag*
      #f
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
                                 (equal? (pid->cwd pid) this-toppath))
                               all-pids)))

        (if (null? our-pids)
            (inote "No mtest or dboard processes on this host in in this runarea.")
            (begin
              (iwarn "Killing all megatest and dashboard processes on this host.")
              (gracefully-kill-pids our-pids)))
        )))

(kill-mtest-dboard)


;;;; delete /tmp/.$USER-portlogger.db
(let ((plfile (conc "/tmp/."*user* "-portlogger.db")))

  (if (safe-file-exists? plfile)
    (if *diag*
        (print "plfile exists - "plfile)
        (begin
          (inote "removing portlogger file")
          (system (conc "rm "plfile))))))


;;;; move logs dir aside
(when (not *diag*)
  (system (conc "mv logs logs-aside-`date +%s`"))
  (system "mkdir logs"))
  

;;;; fixes for dependency diagram
(when (not *diag*)
  (inote "Removing dep graph tmp files if they exist")
  (system (conc "rm /tmp/."*user*"-*.dot"))

  ;;#ln -s /p/fdk/gwa/$USER/fossil/ext/<your flow>_ext ext
  (let* ((toppath (pid->cwd (current-process-id)))
         (flow (car (string-split
                     (car (reverse (string-split toppath "/")))
                     ".")))
         (extdir (conc "/p/fdk/gwa/"*user*
                       "/fossil/ext/"flow"_ext")))
    (when (and (safe-file-exists? extdir)
               (not (safe-file-exists? "ext")))
      (inote "Linking in ext dir")
      (system (conc "ln -s "extdir" ext")))))
  

;;;; check for 0 byte megatest{,_ref}.db in tmp.  delete them
;;;; check for wal-mode megatest{,_ref}.db in tmp.  delete them
(define (repair-dbs)
  (let* ((this-toppath (pid->cwd (current-process-id)))
         (tmppath      (toppath->tmppath this-toppath))
         (golden-mtest-file (conc this-toppath "/megatest.db"))
         (golden-mtest-file-ok (check-db "megatest.db"))
         (tmp-mtest-file    (conc tmppath "/megatest.db"))
         (tmp-mtestref-file    (conc tmppath "/megatest_ref.db"))
         (tmp-mtest-file-ok (check-db tmp-mtest-file))
         (tmp-mtestref-file-ok (check-db tmp-mtestref-file))
         )
;;;; check for megatest{,_ref}.db in tmp that die on .schema.  delete them
    (when (safe-file-exists? tmppath)
        (if tmp-mtest-file-ok
            (inote "tmp megatest db file ok")
            (if *diag*
                (print "diag: tmp megatest db broken - "tmp-mtest-file)
                (delete-db tmp-mtest-file)))
        (if tmp-mtestref-file-ok
            (inote "tmp megatestref db file ok")
            (if *diag*
                (print "diag: tmpref megatest db broken - "tmp-mtestref-file)
                (delete-db tmp-mtestref-file))))

;;;; check for megatest.db
    (if golden-mtest-file-ok
        (inote "golden megatest db file ok")
        (if (not (file-exists? golden-mtest-file))
            (inote "megatest.db not present.  Continuing.")
            (begin
          ;;;; if golden megatest db is broken, stop now!
              (ierr "Golden megatest.db is broken.  Please delete it or replace it from a backup version in .snapshot.  If critical, contact env team to assist.")
              (sendmail "bjbarcla" "!!Bad golden megatest.db" this-toppath)
              (inote "Backups in .snapshot:")
              (system "ls -l .snapshot/*/megatest.db")
              (ierr "Not proceeding with any more checks.")
              (exit 3))))



    ))

(repair-dbs)
    







