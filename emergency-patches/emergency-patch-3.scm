 ;; To build patch:
 ;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; ldd /p/foundry/env/pkgs/megatest/1.64/19/bin/.11/mtest
 ;;        linux-vdso.so.1 =>  (0x00002aaaaaaab000)
 ;;        libchicken.so.7 => /p/foundry/env/pkgs/megatest/1.64/chicken-4.10.0//lib/libchicken.so.7 (0x00002aaaaaaad000)
 ;;        libm.so.6 => /lib64/libm.so.6 (0x00002aaaab0a6000)
 ;;        libdl.so.2 => /lib64/libdl.so.2 (0x00002aaaab31f000)
 ;;        libc.so.6 => /lib64/libc.so.6 (0x00002aaaab523000)
 ;;        /lib64/ld-linux-x86-64.so.2 (0x0000555555554000)
 ;;
 ;;  /p/foundry/env/pkgs/megatest/1.64/chicken-4.10.0/bin/csc -s emergency-patch-3.scm
 ;;


 ;; to test patch:
 ;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; in .megatestrc, add:
 ;; (if (and (> megatest-version 1.64)
 ;;         (< megatest-version 1.6421))
 ;;   (begin
 ;;      (load "/p/foundry/env/pkgs/megatest/1.64/19/share/epatch-1.so")
 ;;      (load "/p/foundry/env/pkgs/megatest/1.64/19/share/epatch-2.so"))) 
 ;;


 ;; to productize patch:
 ;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; 
(use directory-utils regex)

(include "common_records.scm")
(include "key_records.scm")
(include "db_records.scm")
(include "run_records.scm")
(include "test_records.scm")

;; Given a run id start a server process    ### NOTE ### > file 2>&1 
;; if the run-id is zero and the target-host is set 
;; try running on that host
;;   incidental: rotate logs in logs/ dir.
;;
(define  (server:run areapath) ;; areapath is *toppath* for a given testsuite area
  (let* ((curr-host   (get-host-name))
         ;; (attempt-in-progress (server:start-attempted? areapath))
         ;; (dot-server-url (server:check-if-running areapath))
	 (curr-ip     (server:get-best-guess-address curr-host))
	 (curr-pid    (current-process-id))
	 (homehost    (common:get-homehost)) ;; configf:lookup *configdat* "server" "homehost" ))
	 (target-host (car homehost))
	 (testsuite   (common:get-testsuite-name))
	 (logfile     (conc areapath "/logs/server.log")) ;; -" curr-pid "-" target-host ".log"))
	 (cmdln (conc (common:get-megatest-exe)
		      " -server " (or target-host "-") (if (equal? (configf:lookup *configdat* "server" "daemonize") "yes")
							   " -daemonize "
							   "")
		      ;; " -log " logfile
		      " -m testsuite:" testsuite)) ;; (conc " >> " logfile " 2>&1 &")))))
	 (log-rotate  (make-thread common:rotate-logs  "server run, rotate logs thread"))
         (load-limit  (configf:lookup-number *configdat* "server" "load-limit" default: 0.9)))
    ;; we want the remote server to start in *toppath* so push there
    (push-directory areapath)
    (debug:print 0 *default-log-port* "INFO: Trying to start server (" cmdln ") ...")
    (thread-start! log-rotate)
    
    ;; host.domain.tld match host?
    (if (and target-host 
	     ;; look at target host, is it host.domain.tld or ip address and does it 
	     ;; match current ip or hostname
	     (not (string-match (conc "("curr-host "|" curr-host"\\..*)") target-host))
	     (not (equal? curr-ip target-host)))
	(begin
	  (debug:print-info 0 *default-log-port* "Starting server on " target-host ", logfile is " logfile)
	  (setenv "TARGETHOST" target-host)))
      
    (setenv "TARGETHOST_LOGF" logfile)
    (common:wait-for-normalized-load load-limit " delaying server start due to load" remote-host: (get-environment-variable "TARGETHOST")) ;; do not try starting servers on an already overloaded machine, just wait forever
    (system (conc "nbfake " cmdln))
    (unsetenv "TARGETHOST_LOGF")
    (if (get-environment-variable "TARGETHOST")(unsetenv "TARGETHOST"))
    (thread-join! log-rotate)
    (pop-directory)))
