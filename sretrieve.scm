
;; Copyright 2006-2013, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

(use defstruct)

;; (use ssax)
;; (use sxml-serializer)
;; (use sxml-modifications)
;; (use regex)
;; (use srfi-69)
;; (use regex-case)
;; (use posix)
;; (use json)
;; (use csv)
;; (use directory-utils)
(use srfi-18)
(use format)

;; (require-library ini-file)
;; (import (prefix ini-file ini:))

(use sql-de-lite srfi-1 posix regex regex-case srfi-69)
;; (import (prefix sqlite3 sqlite3:))
;; 
(declare (uses configf))
;; (declare (uses tree))
(declare (uses margs))
;; (declare (uses dcommon))
;; (declare (uses launch))
;; (declare (uses gutils))
;; (declare (uses db))
;; (declare (uses synchash))
;; (declare (uses server))
(declare (uses megatest-version))
;; (declare (uses tbd))

(include "megatest-fossil-hash.scm")

;;
;; GLOBALS
;;
(define *verbosity* 1)
(define *logging* #f)
(define *exe-name* (pathname-file (car (argv))))
(define *sretrieve:current-tab-number* 0)
(define *args-hash* (make-hash-table))
(define sretrieve:help (conc "Usage: " *exe-name* " [action [params ...]]

  ls                     : list contents of target area
  get <relversion>       : retrieve data for release <version>
    -m \"message\"       : why retrieved?
  cp <relative path>     : copy file to current directory 
  log                    : get listing of recent downloads

Part of the Megatest tool suite.
Learn more at http://www.kiatoa.com/fossils/megatest

Version: " megatest-fossil-hash)) ;; "

;;======================================================================
;; RECORDS
;;======================================================================

;;======================================================================
;; DB
;;======================================================================

;; replace (strftime('%s','now')), with datetime('now'))
(define (sretrieve:initialize-db db)
  (for-each
   (lambda (qry)
     (exec (sql db qry)))
   (list 
    "CREATE TABLE IF NOT EXISTS actions
         (id           INTEGER PRIMARY KEY,
          action       TEXT NOT NULL,
          retriever    TEXT NOT NULL,
          datetime     TIMESTAMP DEFAULT (datetime('now','localtime')),
          srcpath      TEXT NOT NULL,
          comment      TEXT DEFAULT '' NOT NULL,
          state        TEXT DEFAULT 'new');"
    "CREATE TABLE IF NOT EXISTS bundles
         (id           INTEGER PRIMARY KEY,
          bundle       TEXT NOT NULL,
          release      TEXT NOT NULL,
          status       TEXT NOT NULL,
          event_date   TEXT NOT NULL);"
    )))

(define (sretrieve:register-action db action submitter source-path comment)
  (print "(sretrieve:register-action db " db " action " action " submitter " submitter " source-path " source-path " comment " comment)
  (exec (sql db "INSERT INTO actions (action,retriever,srcpath,comment)
                 VALUES(?,?,?,?)")
	action
	submitter
	source-path
	(or comment "")))

;; (call-with-database
;;  (lambda (db)
;;   (set-busy-handler! db (busy-timeout 10000)) ; 10 second timeout
;;   ...))

;; Create the sqlite db
(define (sretrieve:db-do configdat proc) 

  (let ((path (configf:lookup configdat "database" "location")))
    (if (not path)
	(begin
	  (debug:print 0 "[database]\nlocation /some/path\n\n Is missing from the config file!")
	  (exit 1)))
    (if (and path
	     (directory? path)
	     (file-read-access? path))
	(let* ((dbpath    (conc path "/" *exe-name* ".db"))
	       (writeable (file-write-access? dbpath))
	       (dbexists  (file-exists? dbpath)))
	  (handle-exceptions
	   exn
	   (begin
	     (debug:print 2 "ERROR: problem accessing db " dbpath
			  ((condition-property-accessor 'exn 'message) exn))
	     (exit 1))
            ;;(debug:print 0 "calling proc " proc "db path " dbpath )
	   (call-with-database
            dbpath
	    (lambda (db)
	       ;;(debug:print 0 "calling proc " proc " on db " db)
	      (set-busy-handler! db (busy-timeout 10000)) ;; 10 sec timeout
	      (if (not dbexists)(sretrieve:initialize-db db))
	      (proc db)))))
	(debug:print 0 "ERROR: invalid path for storing database: " path))))

;; copy in directory to dest, validation is done BEFORE calling this
;;
(define (sretrieve:get configdat retriever version comment)
  (let* ((base-dir  (configf:lookup configdat "settings" "base-dir"))
	 (datadir   (conc base-dir "/" version)))
    (if (or (not base-dir)
	    (not (file-exists? base-dir)))
	(begin
	  (debug:print 0 "ERROR: Bad configuration! base-dir " base-dir " not found")
	  (exit 1)))
    (print datadir)
    (if (not (file-exists? datadir))
	(begin
	  (debug:print 0 "ERROR: Bad version (" version "), no data found at " datadir "." )
	  (exit 1)))
    
    (sretrieve:db-do
     configdat
     (lambda (db)
       (sretrieve:register-action db "get" retriever datadir comment)))
      (sretrieve:do-as-calling-user
       (lambda ()
         (if (directory? datadir)
	   (begin
  	    (change-directory datadir)
	    (let ((files (filter (lambda (x)
				(not (member x '("." ".."))))
			      (glob "*" ".*"))))
	     (print "files: " files)
	     (process-execute "/bin/tar" (append (append (list  "chfv" "-") files) (list "--ignore-failed-read")))))
             (begin
               (let* ((parent-dir (pathname-directory datadir) )
                      (filename  (conc(pathname-file datadir) "." (pathname-extension datadir))))
                  (change-directory parent-dir)  
                  (process-execute "/bin/tar" (list "chfv" "-" filename))
             )))
))
))


;; copy in file to dest, validation is done BEFORE calling this
;;
(define (sretrieve:cp configdat retriever file comment)
  (let* ((base-dir  (configf:lookup configdat "settings" "base-dir"))
         (allowed-sub-paths (configf:lookup configdat "settings" "allowed-sub-paths"))    
	 (datadir   (conc base-dir "/" file))
         (filename  (conc(pathname-file datadir) "." (pathname-extension datadir))))
    (if (or (not base-dir)
	    (not (file-exists? base-dir)))
	(begin
	  (debug:print 0 "ERROR: Bad configuration! base-dir " base-dir " not found")
	  (exit 1)))
    (print datadir)
    (if (not (file-exists? datadir))
	(begin
	  (debug:print 0 "ERROR: File  (" file "), not found at " base-dir "." )
	  (exit 1)))
    (if (directory? datadir)
	(begin
	  (debug:print 0 "ERROR: (" file ") is a dirctory!! cp cmd works only on files ." )
	  (exit 1)))
    (if(not (string-match (regexp  allowed-sub-paths) file))
        (begin
	  (debug:print 0 "ERROR: Access denied to file (" file ")!! " )
	  (exit 1)))
     
     (sretrieve:db-do
     configdat
     (lambda (db)
       (sretrieve:register-action db "cp" retriever datadir comment)))
      (sretrieve:do-as-calling-user
      ;;  (debug:print 0 "ph:  "(pathname-directory datadir)  "!! " )
       (change-directory (pathname-directory datadir))  
       ;;(debug:print 0 "ph: /bin/tar" (list "chfv" "-" filename) )
      (process-execute "/bin/tar" (list "chfv" "-" filename)))
      ))

;; ls in file to dest, validation is done BEFORE calling this
;;
(define (sretrieve:ls configdat retriever file comment)
  (let* ((base-dir  (configf:lookup configdat "settings" "base-dir"))
         (allowed-sub-paths (configf:lookup configdat "settings" "allowed-sub-paths"))    
	 (datadir   (conc base-dir "/" file))
         (filename  (conc(pathname-file datadir) "." (pathname-extension datadir))))
    (if (or (not base-dir)
	    (not (file-exists? base-dir)))
	(begin
	  (debug:print 0 "ERROR: Bad configuration! base-dir " base-dir " not found")
	  (exit 1)))
    (print datadir)
    (if (not (file-exists? datadir))
	(begin
	  (debug:print 0 "ERROR: File  (" file "), not found at " base-dir "." )
	  (exit 1)))
      (if(not (string-match (regexp  allowed-sub-paths) file))
        (begin
	  (debug:print 0 "ERROR: Access denied to file (" file ")!! " )
	  (exit 1)))
   
        (sretrieve:do-as-calling-user
        (lambda ()
	 ;;(change-directory datadir)
         ;; (debug:print 0  "/usr/bin/find" (list datadir "-ls" "|" "grep" "-E" "'"allowed-file-patt"'"))
         ;; (status (with-input-from-pipe "find " datadir " -ls | grep -E '" allowed-file-patt "'" (lambda () (read-line))))
         ;; (debug:print 0 status) 
	  (process-execute "/bin/ls" (list "-ls"  "-lrt" datadir ))
 ))))



;;(filter (lambda (x)
;;							     (not (member x '("." ".."))))
;;							   (glob "*" ".*"))))))))

(define (sretrieve:validate target-dir targ-mk)
  (let* ((normal-path (normalize-pathname targ-mk))
        (targ-path (conc target-dir "/" normal-path)))
    (if (string-contains   normal-path "..")
    (begin
      (debug:print 0 "ERROR: Path  " targ-mk " resolved outside target area "  target-dir )
      (exit 1)))

    (if (not (string-contains targ-path target-dir))
    (begin
      (debug:print 0 "ERROR: You cannot update data outside " target-dir ".")
      (exit 1)))
    (debug:print 0 "Path " targ-mk " is valid.")   
 ))
;; make directory in dest
;;

(define (sretrieve:mkdir configdat submitter target-dir targ-mk comment)
  (let ((targ-path (conc target-dir "/" targ-mk)))
    
    (if (file-exists? targ-path)
	(begin
	  (debug:print 0 "ERROR: target Directory " targ-path " already exist!!")
	  (exit 1)))
    (sretrieve:db-do
     configdat
     (lambda (db)
       (sretrieve:register-action db "mkdir" submitter targ-mk comment)))
    (let* ((th1         (make-thread
			 (lambda ()
			   (create-directory targ-path #t)
			   (debug:print 0 " ... dir " targ-path " created"))
			 "mkdir thread"))
	   (th2         (make-thread
			 (lambda ()
			   (let loop ()
			     (thread-sleep! 15)
			     (display ".")
			     (flush-output)
			     (loop)))
			 "action is happening thread")))
      (thread-start! th1)
      (thread-start! th2)
      (thread-join! th1))
    (cons #t "Successfully saved data")))

;; create a symlink in dest
;;
(define (sretrieve:ln configdat submitter target-dir targ-link link-name comment)
  (let ((targ-path (conc target-dir "/" link-name)))
    (if (file-exists? targ-path)
	(begin
	  (debug:print 0 "ERROR: target file " targ-path " already exist!!")
	  (exit 1)))
     (if (not (file-exists? targ-link ))
	(begin
	  (debug:print 0 "ERROR: target file " targ-link " does not exist!!")
	  (exit 1)))
 
    (sretrieve:db-do
     configdat
     (lambda (db)
       (sretrieve:register-action db "ln" submitter link-name comment)))
    (let* ((th1         (make-thread
			 (lambda ()
			   (create-symbolic-link targ-link targ-path  )
			   (debug:print 0 " ... link " targ-path " created"))
			 "symlink thread"))
	   (th2         (make-thread
			 (lambda ()
			   (let loop ()
			     (thread-sleep! 15)
			     (display ".")
			     (flush-output)
			     (loop)))
			 "action is happening thread")))
      (thread-start! th1)
      (thread-start! th2)
      (thread-join! th1))
    (cons #t "Successfully saved data")))


;; remove copy of file in dest
;;
(define (sretrieve:rm configdat submitter target-dir targ-file comment)
  (let ((targ-path (conc target-dir "/" targ-file)))
    (if (not (file-exists? targ-path))
	(begin
	  (debug:print 0 "ERROR: target file " targ-path " not found, nothing to remove.")
	  (exit 1)))
    (sretrieve:db-do
     configdat
     (lambda (db)
       (sretrieve:register-action db "rm" submitter targ-file comment)))
    (let* ((th1         (make-thread
			 (lambda ()
			   (delete-file targ-path)
			   (debug:print 0 " ... file " targ-path " removed"))
			 "rm thread"))
	   (th2         (make-thread
			 (lambda ()
			   (let loop ()
			     (thread-sleep! 15)
			     (display ".")
			     (flush-output)
			     (loop)))
			 "action is happening thread")))
      (thread-start! th1)
      (thread-start! th2)
      (thread-join! th1))
    (cons #t "Successfully saved data")))

(define (sretrieve:backup-move path)
  (let* ((trashdir  (conc (pathname-directory path) "/.trash"))
	 (trashfile (conc trashdir "/" (current-seconds) "-" (pathname-file path))))
    (create-directory trashdir #t)
    (if (directory? path)
	(system (conc "mv " path " " trashfile))
	(file-move path trash-file))))


(define (sretrieve:lst->path pathlst)
  (conc "/" (string-intersperse (map conc pathlst) "/")))

(define (sretrieve:path->lst path)
  (string-split path "/"))

(define (sretrieve:pathdat-apply-heuristics configdat path)
  (cond
   ((file-exists? path) "found")
   (else (conc path " not installed"))))

;;======================================================================
;; MISC
;;======================================================================

(define (sretrieve:do-as-calling-user proc)
  (let ((eid (current-effective-user-id))
        (cid (current-user-id)))
    (if (not (eq? eid cid)) ;; running suid
            (set! (current-effective-user-id) cid))
    ;; (debug:print 0 "running as " (current-effective-user-id))
    (proc)
    (if (not (eq? eid cid))
        (set! (current-effective-user-id) eid))))

(define (sretrieve:find name paths)
  (if (null? paths)
      #f
      (let loop ((hed (car paths))
		 (tal (cdr paths)))
	(if (file-exists? (conc hed "/" name))
	    hed
	    (if (null? tal)
		#f
		(loop (car tal)(cdr tal)))))))

(define (sretrieve:stderr-print . args)
  (with-output-to-port (current-error-port)
    (lambda ()
      (apply print args))))

;;======================================================================
;; MAIN
;;======================================================================

(define (sretrieve:load-config exe-dir exe-name)
  (let* ((fname   (conc exe-dir "/." exe-name ".config")))
    ;; (ini:property-separator-patt " *  *")
    ;; (ini:property-separator #\space)
    (if (file-exists? fname)
	;; (ini:read-ini fname)
	(read-config fname #f #t)
	(make-hash-table))))

;; package-type is "megatest", "builds", "kits" etc.
;;
(define (sretrieve:load-packages configdat exe-dir package-type)
  (push-directory exe-dir)
  (let* ((packages-metadir  (configf:lookup configdat "settings" "packages-metadir"))
	 (conversion-script (configf:lookup configdat "settings" "conversion-script"))
	 (upstream-file     (configf:lookup configdat "settings" "upstream-file"))
	 (package-config    (conc packages-metadir "/" package-type ".config")))
    ;; this section here does a timestamp based rebuild of the
    ;;   <packages-metadir>/<package-type>.config file using
    ;;   <upstream-file> as an input
    (if (file-exists? upstream-file)
	(if (or (not (file-exists? package-config)) ;; if not created call the updater, otherwise call only if upstream newer
		(> (file-modification-time upstream-file)(file-modification-time package-config)))
	    (handle-exceptions
	     exn
	     (debug:print 0 "ERROR: failed to run script " conversion-script " with params " upstream-file " " package-config)
	     (let ((pid (process-run conversion-script (list upstream-file package-config))))
	       (process-wait pid)))
	    (debug:print 0 "Skipping update of " package-config " from " upstream-file))
	(debug:print 0 "Skipping update of " package-config " as " upstream-file " not found"))
    ;; (ini:property-separator-patt " *  *")
    ;; (ini:property-separator #\space)
    (let ((res (if (file-exists? package-config)
		   (begin
		     (debug:print 0 "Reading package config " package-config)
		     (read-config package-config #f #t))
		   (make-hash-table))))
      (pop-directory)
      res)))

(define (sretrieve:process-action configdat action . args)
  (let* ((base-dir      (configf:lookup configdat "settings" "base-dir"))
	 (user          (current-user-name))
         (allowed-sub-paths (configf:lookup configdat "settings" "allowed-sub-paths")) 
	 (allowed-users (string-split
			 (or (configf:lookup configdat "settings" "allowed-users")
			     "")))
	 (default-area  (configf:lookup configdat "settings" "default-area"))) ;; otherwise known as the package
    
    (if (not base-dir)
	(begin
	  (debug:print 0 "[settings]\nbase-dir /some/path\n\n Is MISSING from the config file!")
	  (exit)))
    (if (null? allowed-users)
	(begin
	  (debug:print 0 "[setings]\nallowed-users user1 user2 ...\n\n Is MISSING from the config file!")
	  (exit)))
    (if (not (member user allowed-users))
	(begin
	  (debug:print 0 "User \"" (current-user-name) "\" does not have access. Exiting")
	  (exit 1)))
    (case (string->symbol action)
      ((get)
       (if (< (length args) 1)
	   (begin 
	     (debug:print 0 "ERROR: Missing arguments; " (string-intersperse args ", "))
	     (exit 1)))
       (let* ((remargs     (args:get-args args '("-m" "-i" "-package") '() args:arg-hash 0))
              (version     (car args))
	      (msg         (or (args:get-arg "-m") ""))
	      (package-type (or (args:get-arg "-package")
				default-area))
	      (exe-dir     (configf:lookup configdat "exe-info" "exe-dir")))
;;	      (relconfig   (sretrieve:load-packages configdat exe-dir package-type)))

	 (debug:print 0 "retrieving " version " of " package-type " as tar data on stdout")
	 (sretrieve:get configdat user version msg)))
         ((cp)
            (if (< (length args) 1)
             (begin 
	     (debug:print 0 "ERROR: Missing arguments; " (string-intersperse args ", "))
	     (exit 1)))
          (let* ((remargs     (args:get-args args '("-m" "-i" "-package") '() args:arg-hash 0))
              (file     (car args))
	      (msg         (or (args:get-arg "-m") "")) )

	 (debug:print 0 "copinging " file " to current directory " )
	 (sretrieve:cp configdat user file msg)))
      ((ls)
            (if (< (length args) 1)
             (begin 
	     (debug:print 0 "ERROR: Missing arguments; " (string-intersperse args ", "))
	     (exit 1)))
          (let* ((remargs     (args:get-args args '("-m" "-i" "-package") '() args:arg-hash 0))
              (dir     (car args))
	      (msg         (or (args:get-arg "-m") "")) )

	 (debug:print 0 "Listing files in " )
	 (sretrieve:ls configdat user dir msg)))
 
      (else (debug:print 0 "Unrecognised command " action)))))
  
;; ease debugging by loading ~/.dashboardrc - REMOVE FROM PRODUCTION!
;; (let ((debugcontrolf (conc (get-environment-variable "HOME") "/.sretrieverc")))
;;   (if (file-exists? debugcontrolf)
;;       (load debugcontrolf)))

(define (main)
  (let* ((args      (argv))
	 (prog      (car args))
	 (rema      (cdr args))
	 (exe-name  (pathname-file (car (argv))))
	 (exe-dir   (or (pathname-directory prog)
			(sretrieve:find exe-name (string-split (get-environment-variable "PATH") ":"))))
	 (configdat (sretrieve:load-config exe-dir exe-name)))
    ;; preserve the exe data in the config file
    (hash-table-set! configdat "exe-info" (list (list "exe-name" exe-name)
						(list "exe-dir"  exe-dir)))
    (cond
     ;; one-word commands
     ((eq? (length rema) 1)
      (case (string->symbol (car rema))
	((help -h -help --h --help)
	 (print sretrieve:help))
	((list-vars) ;; print out the ini file
	 (map print (sretrieve:get-areas configdat)))
	((ls)
	 (let* ((base-dir (configf:lookup configdat "settings" "base-dir")))
	   (if base-dir
	       (begin
		 (print "Files in " base-dir)
                 (sretrieve:do-as-calling-user
                    (lambda ()
		 (process-execute "/bin/ls" (list "-lrt" base-dir)))))
	       (print "ERROR: No base dir specified!"))))
	((log)
	 (sretrieve:db-do configdat (lambda (db)
				     (print "Logs : ")
				     (query (for-each-row
					     (lambda (row)
					       (apply print (intersperse row " | "))))
					    (sql db "SELECT * FROM actions")))))
	(else
	 (print "ERROR: Unrecognised command. Try \"sretrieve help\""))))
     ;; multi-word commands
     ((null? rema)(print sretrieve:help))
     ((>= (length rema) 2)
      (apply sretrieve:process-action configdat (car rema)(cdr rema)))
     (else (debug:print 0 "ERROR: Unrecognised command. Try \"sretrieve help\"")))))

(main)
