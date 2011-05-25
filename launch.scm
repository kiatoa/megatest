
;; Copyright 2006-2011, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

;;======================================================================
;; launch a task - this runs on the originating host, tests themselves
;;
;;======================================================================

(define (setup-for-run)
  (set! *configinfo* (find-and-read-config (if (args:get-arg "-config")(args:get-arg "-config") "megatest.config")))
  (set! *configdat*  (if (car *configinfo*)(car *configinfo*) #f))
  (set! *toppath*    (if (car *configinfo*)(cadr *configinfo*) #f))
  (if *toppath*
      (setenv "MT_RUN_AREA_HOME" *toppath*) ;; to be deprecated
      (print "ERROR: failed to find the top path to your run setup."))
  *toppath*)

(define (setup-env-defaults db fname run-id . already-seen)
  (let* ((keys    (get-keys db))
	 (keyvals (get-key-vals db run-id))
	 (thekey  (string-intersperse (map (lambda (x)(if x x "-na-")) keyvals) "/"))
	 (confdat (read-config fname))
	 (whatfound (make-hash-table))
	 (sections (list "default" thekey)))
    ;; (print "Using key=\"" thekey "\"")
    (for-each 
     (lambda (section)
       (let ((section-dat (hash-table-ref/default confdat section #f)))
	 (if section-dat
	     (for-each 
	      (lambda (envvar)
		(hash-table-set! whatfound section (+ (hash-table-ref/default whatfound section 0) 1))
		(setenv envvar (cadr (assoc envvar section-dat))))
	      (map car section-dat)))))
     sections)
    (if (and (not (null? already-seen))
	     (not (car already-seen)))
	(begin
	  (print "Key settings found in runconfig.config:")
	  (for-each (lambda (fullkey)
		      (format #t "~20a ~a\n" fullkey (hash-table-ref/default whatfound fullkey 0)))
		    sections)
	  (print "---")
	  (set! *already-seen-runconfig-info* #t)))))

(define (get-best-disk confdat)
  (let* ((disks    (hash-table-ref/default confdat "disks" #f))
	 (best     #f)
	 (bestsize 0))
    (if disks 
	(for-each 
	 (lambda (disk-num)
	   (let* ((dirpath    (cadr (assoc disk-num disks)))
		  (freespc    (if (directory? dirpath)
				  (get-df dirpath)
				  (begin
				    (print "WARNING: path " dirpath " in [disks] section not valid")
				    0))))
	     (if (> freespc bestsize)
		 (begin
		   (set! best     dirpath)
		   (set! bestsize freespc)))))
	 (map car disks)))
    best))

(define (create-work-area db run-id test-path disk-path testname itemdat)
  (let* ((run-info (db-get-run-info db run-id))
	 (item-path (let ((ip (item-list->path itemdat)))
		      (if (equal? ip "") "" (conc "/" ip))))
	 (runname  (db-get-value-by-header (db:get-row run-info)
					   (db:get-header run-info)
					   "runname"))
	 (key-vals (get-key-vals db run-id))
	 (key-str  (string-intersperse key-vals "/"))
	 (dfullp   (conc disk-path "/" key-str "/" runname "/" testname
			 item-path))
	 (toptest-path (conc disk-path "/" key-str "/" runname "/" testname))
	 (runsdir  (config-lookup *configdat* "setup" "runsdir"))
	 (lnkpath  (conc (if runsdir runsdir (conc *toppath* "/runs"))
			 "/" key-str "/" runname item-path)))
    ;; since this is an iterated test this is as good a place as any to
    ;; update the toptest record with its location rundir
    (if (not (equal? item-path ""))
	(db:test-set-rundir! db run-id testname "" toptest-path))
    (print "Setting up test run area")
    (print " - creating run area in " dfullp)
    (system  (conc "mkdir -p " dfullp))
    (print " - creating link from " dfullp "/" testname " to " lnkpath)
    (system  (conc "mkdir -p " lnkpath))
    (if (file-exists? (conc lnkpath "/" testname))
	(system (conc "rm -f " lnkpath "/" testname)))
    (system  (conc "ln -sf " dfullp " " lnkpath "/" testname))
    (if (directory? dfullp)
	(begin
	  (system  (conc "rsync -av " test-path "/ " dfullp "/"))
	  (list dfullp toptest-path))
	(list #f #f))))

;; 1. look though disks list for disk with most space
;; 2. create run dir on disk, path name is meaningful
;; 3. create link from run dir to megatest runs area 
;; 4. remotely run the test on allocated host
;;    - could be ssh to host from hosts table (update regularly with load)
;;    - could be netbatch
;;      (launch-test db (cadr status) test-conf))
(define (launch-test db run-id test-conf keyvallst test-name test-path itemdat)
  (change-directory *toppath*)
  (let ((launcher   (config-lookup *configdat* "jobtools"     "launcher"))
	(runscript  (config-lookup test-conf   "setup"        "runscript"))
	(diskspace  (config-lookup test-conf   "requirements" "diskspace"))
	(memory     (config-lookup test-conf   "requirements" "memory"))
	(hosts      (config-lookup *configdat* "jobtools"     "workhosts"))
	(remote-megatest (config-lookup *configdat* "setup" "executable"))
	(local-megatest  (car (argv)))
	;; (item-path  (item-list->path itemdat)) test-path is the full path including the item-path
	(work-area  #f)
	(toptest-work-area #f) ;; for iterated tests the top test contains data relevant for all
	(diskpath   #f)
	(cmdparms   #f)
	(fullcmd    #f) ;; (define a (with-output-to-string (lambda ()(write x))))
	(mt-bindir-path #f))
    (if hosts (set! hosts (string-split hosts)))
    (if (not remote-megatest)(set! remote-megatest local-megatest)) ;; "megatest"))
    (set! mt-bindir-path (pathname-directory remote-megatest))
    (if launcher (set! launcher (string-split launcher)))
    ;; set up the run work area for this test
    (set! diskpath (get-best-disk *configdat*))
    (if diskpath
	(let ((dat  (create-work-area db run-id test-path diskpath test-name itemdat)))
	  (set! work-area (car dat))
	  (set! toptest-work-area (cadr dat)))
	(begin
	  (set! work-area test-path)
	  (print "WARNING: No disk work area specified - running in the test directory")))
    (set! cmdparms (base64:base64-encode (with-output-to-string
				    (lambda () ;; (list 'hosts     hosts)
				      (write (list (list 'testpath  test-path)
						   (list 'work-area work-area)
						   (list 'test-name test-name) 
						   (list 'runscript runscript) 
						   (list 'run-id    run-id   )
						   (list 'itemdat   itemdat  )
						   (list 'megatest  remote-megatest)
						   (list 'env-ovrd  (hash-table-ref/default *configdat* "env-override" '()))
						   (list 'runname   (args:get-arg ":runname"))
						   (list 'mt-bindir-path mt-bindir-path))))))) ;; (string-intersperse keyvallst " "))))
    (change-directory work-area) ;; so that log files from the launch process don't clutter the test dir
    (cond
     ((and launcher hosts) ;; must be using ssh hostname
      (set! fullcmd (append launcher (car hosts)(list remote-megatest "-execute" cmdparms))))
     (launcher
      (set! fullcmd (append launcher (list remote-megatest "-execute" cmdparms))))
     (else
      (set! fullcmd (list remote-megatest "-execute" cmdparms))))
    (if (args:get-arg "-xterm")(set! fullcmd (append fullcmd (list "-xterm"))))
    (print "Launching megatest for test " test-name " in " work-area" ...")
    (test-set-status! db run-id test-name "LAUNCHED" "n/a" itemdat) ;; (if launch-results launch-results "FAILED"))
    ;; set 
    ;; set pre-launch-env-vars before launching, keep the vars in prevvals and put the envionment back when done
    (let* ((commonprevvals (alist->env-vars
			    (hash-table-ref/default *configdat* "env-override" '())))
	   (testprevvals   (alist->env-vars
			    (hash-table-ref/default test-conf "pre-launch-env-overrides" '())))
	   (miscprevvals   (alist->env-vars ;; consolidate this code with the code in megatest.scm for "-execute"
			    (append (list (list "MT_TEST_NAME" test-name)
					  (list "MT_ITEM_INFO" (conc itemdat)) 
					  (list "MT_RUNNAME"   (args:get-arg ":runname")))
				    itemdat)))
	   (launch-results (apply cmd-run-proc-each-line
				  (car fullcmd)
				  print
				  (cdr fullcmd)))) ;;  launcher fullcmd)));; (apply cmd-run-proc-each-line launcher print fullcmd))) ;; (cmd-run->list fullcmd))
      (print "Launching completed, updating db")
      (alist->env-vars miscprevvals)
      (alist->env-vars testprevvals)
      (alist->env-vars commonprevvals))))

