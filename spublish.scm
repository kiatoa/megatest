
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
(use srfi-18)
(use format)

(require-library ini-file)
(import (prefix ini-file ini:))

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
(define *spublish:current-tab-number* 0)
(define *args-hash* (make-hash-table))
(define spublish:help (conc "Usage: spublish [action [params ...]]

  ls                     : list contents of target area
  cp|publish <src file> <relative dest>      : copy file to target area
  mkdir <dir name>       : maks directory in target area  
  rm <file>              : remove file <file> from target area
  ln <target> <link name> : creates a symlink
  log                    :

  options:

    -m \"message\"        : describe what was done

Part of the Megatest tool suite.
Learn more at http://www.kiatoa.com/fossils/megatest

Version: " megatest-fossil-hash)) ;; "

;;======================================================================
;; RECORDS
;;======================================================================

;;======================================================================
;; DB
;;======================================================================

(define (spublish:initialize-db db)
  (for-each
   (lambda (qry)
     (exec (sql db qry)))
   (list 
    "CREATE TABLE IF NOT EXISTS actions
         (id           INTEGER PRIMARY KEY,
          action       TEXT NOT NULL,
          submitter    TEXT NOT NULL,
          datetime     TIMESTAMP DEFAULT (strftime('%s','now')),
          srcpath      TEXT NOT NULL,
          comment      TEXT DEFAULT '' NOT NULL,
          state        TEXT DEFAULT 'new');"
    )))

(define (spublish:register-action db action submitter source-path comment)
  (exec (sql db "INSERT INTO actions (action,submitter,srcpath,comment)
                 VALUES(?,?,?,?)")
	action
	submitter
	source-path
	comment))

;; (call-with-database
;;  (lambda (db)
;;   (set-busy-handler! db (busy-timeout 10000)) ; 10 second timeout
;;   ...))

;; Create the sqlite db
(define (spublish:db-do configdat proc) 
  (let ((path (configf:lookup configdat "database" "location")))
    (if (not path)
	(begin
	  (print "[database]\nlocation /some/path\n\n Is missing from the config file!")
	  (exit 1)))
    (if (and path
	     (directory? path)
	     (file-read-access? path))
	(let* ((dbpath    (conc path "/spublish.db"))
	       (writeable (file-write-access? dbpath))
	       (dbexists  (file-exists? dbpath)))
	  (handle-exceptions
	   exn
	   (begin
	     (debug:print 2 "ERROR: problem accessing db " dbpath
			  ((condition-property-accessor 'exn 'message) exn))
	     (exit 1))
	   (call-with-database
            dbpath
	    (lambda (db)
	      ;; (print "calling proc " proc " on db " db)
	      (set-busy-handler! db (busy-timeout 10000)) ;; 10 sec timeout
	      (if (not dbexists)(spublish:initialize-db db))
	      (proc db)))))
	(print "ERROR: invalid path for storing database: " path))))

;; copy in file to dest, validation is done BEFORE calling this
;;
(define (spublish:cp configdat submitter source-path target-dir targ-file dest-dir comment)
  (let ((dest-dir-path (conc target-dir "/" dest-dir))
        (targ-path (conc target-dir "/" dest-dir "/" targ-file)))
    (if (file-exists? targ-path)
	(begin
	  (print "ERROR: target file already exists, remove it before re-publishing")
	  (exit 1)))
       (if (not(file-exists? dest-dir-path))
	(begin
	  (print "ERROR: target directory " dest-dir-path " does not exists." )
	  (exit 1)))

    (spublish:db-do
     configdat
     (lambda (db)
       (spublish:register-action db "cp" submitter source-path comment)))
    (let* (;; (target-path (configf:lookup "settings" "target-path"))
	   (th1         (make-thread
			 (lambda ()
			   (file-copy source-path targ-path #t))
                            (print " ... file " targ-path " copied to" targ-path)
			 ;; (let ((pid (process-run "cp" (list source-path target-dir))))
			 ;;   (process-wait pid)))
			 "copy thread"))
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

(define (spublish:validate target-dir targ-mk)
  (let* ((normal-path (normalize-pathname targ-mk))
        (targ-path (conc target-dir "/" normal-path)))
    (if (string-contains   normal-path "..")
    (begin
      (print "ERROR: Path  " targ-mk " resolved outside target area "  target-dir )
      (exit 1)))

    (if (not (string-contains targ-path target-dir))
    (begin
      (print "ERROR: You cannot update data outside " target-dir ".")
      (exit 1)))
    (print "Path " targ-mk " is valid.")   
 ))
;; make directory in dest
;;

(define (spublish:mkdir configdat submitter target-dir targ-mk comment)
  (let ((targ-path (conc target-dir "/" targ-mk)))
    
    (if (file-exists? targ-path)
	(begin
	  (print "ERROR: target Directory " targ-path " already exist!!")
	  (exit 1)))
    (spublish:db-do
     configdat
     (lambda (db)
       (spublish:register-action db "mkdir" submitter targ-mk comment)))
    (let* ((th1         (make-thread
			 (lambda ()
			   (create-directory targ-path #t)
			   (print " ... dir " targ-path " created"))
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
(define (spublish:ln configdat submitter target-dir targ-link link-name comment)
  (let ((targ-path (conc target-dir "/" link-name)))
    (if (file-exists? targ-path)
	(begin
	  (print "ERROR: target file " targ-path " already exist!!")
	  (exit 1)))
     (if (not (file-exists? targ-link ))
	(begin
	  (print "ERROR: target file " targ-link " does not exist!!")
	  (exit 1)))
 
    (spublish:db-do
     configdat
     (lambda (db)
       (spublish:register-action db "ln" submitter link-name comment)))
    (let* ((th1         (make-thread
			 (lambda ()
			   (create-symbolic-link targ-link targ-path  )
			   (print " ... link " targ-path " created"))
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
(define (spublish:rm configdat submitter target-dir targ-file comment)
  (let ((targ-path (conc target-dir "/" targ-file)))
    (if (not (file-exists? targ-path))
	(begin
	  (print "ERROR: target file " targ-path " not found, nothing to remove.")
	  (exit 1)))
    (spublish:db-do
     configdat
     (lambda (db)
       (spublish:register-action db "rm" submitter targ-file comment)))
    (let* ((th1         (make-thread
			 (lambda ()
			   (delete-file targ-path)
			   (print " ... file " targ-path " removed"))
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

(define (spublish:backup-move path)
  (let* ((trashdir  (conc (pathname-directory path) "/.trash"))
	 (trashfile (conc trashdir "/" (current-seconds) "-" (pathname-file path))))
    (create-directory trashdir #t)
    (if (directory? path)
	(system (conc "mv " path " " trashfile))
	(file-move path trash-file))))


(define (spublish:lst->path pathlst)
  (conc "/" (string-intersperse (map conc pathlst) "/")))

(define (spublish:path->lst path)
  (string-split path "/"))

(define (spublish:pathdat-apply-heuristics configdat path)
  (cond
   ((file-exists? path) "found")
   (else (conc path " not installed"))))

;;======================================================================
;; MISC
;;======================================================================

(define (spublish:do-as-calling-user proc)
  (let ((eid (current-effective-user-id))
        (cid (current-user-id)))
    (if (not (eq? eid cid)) ;; running suid
            (set! (current-effective-user-id) cid))
    ;; (print "running as " (current-effective-user-id))
    (proc)
    (if (not (eq? eid cid))
        (set! (current-effective-user-id) eid))))

(define (spublish:find name paths)
  (if (null? paths)
      #f
      (let loop ((hed (car paths))
		 (tal (cdr paths)))
	(if (file-exists? (conc hed "/" name))
	    hed
	    (if (null? tal)
		#f
		(loop (car tal)(cdr tal)))))))

;;======================================================================
;; MAIN
;;======================================================================

(define (spublish:load-config exe-dir exe-name)
  (let* ((fname   (conc exe-dir "/." exe-name ".config")))
    (ini:property-separator-patt " *  *")
    (ini:property-separator #\space)
    (if (file-exists? fname)
	;; (ini:read-ini fname)
	(read-config fname #f #t)
	(make-hash-table))))

(define (spublish:process-action configdat action . args)
  (let* ((target-dir    (configf:lookup configdat "settings" "target-dir"))
	 (user          (current-user-name))
	 (allowed-users (string-split
			 (or (configf:lookup configdat "settings" "allowed-users")
			     ""))))
    (if (not target-dir)
	(begin
	  (print "[settings]\ntarget-dir /some/path\n\n Is MISSING from the config file!")
	  (exit)))
    (if (null? allowed-users)
	(begin
	  (print "[setings]\nallowed-users user1 user2 ...\n\n Is MISSING from the config file!")
	  (exit)))
    (if (not (member user allowed-users))
	(begin
	  (print "User \"" (current-user-name) "\" does not have access. Exiting")
	  (exit 1)))
    (case (string->symbol action)
      ((cp publish)
       (if (< (length args) 2)
	   (begin 
	     (print "ERROR: Missing arguments; " (string-intersperse args ", "))
	     (exit 1)))
       (let* ((remargs     (args:get-args args '("-m") '() args:arg-hash 0))
              (dest-dir (cadr args))
              (src-path-in (car args))
	      (src-path    (with-input-from-pipe
			    (conc "readlink -f " src-path-in)
			    (lambda ()
			      (read-line))))
	      (msg         (or (args:get-arg "-m") ""))
	      (targ-file   (pathname-strip-directory src-path)))
	 (if (not (file-read-access? src-path))
	     (begin
	       (print "ERROR: source file not readable: " src-path)
	       (exit 1)))
	 (if (directory? src-path)
	     (begin
	       (print "ERROR: source file is a directory, this is not supported yet.")
	       (exit 1)))
	 (print "publishing " src-path-in " to " target-dir)
         (spublish:validate     target-dir dest-dir)
	 (spublish:cp configdat user src-path target-dir targ-file dest-dir msg)))
      ((mkdir)
        (if (< (length args) 1)
          (begin 
	     (print "ERROR: Missing arguments; " (string-intersperse args ", "))
	     (exit 1)))
        (let* ((targ-mk (car args))
               (msg         (or (args:get-arg "-m") ""))) 
               (print "attempting to create directory " targ-mk " in " target-dir)
               (spublish:validate     target-dir targ-mk)
               (spublish:mkdir configdat user target-dir targ-mk msg)))

      ((ln) 
        (if (< (length args) 2)
          (begin 
	     (print "ERROR: Missing arguments; " (string-intersperse args ", "))
	     (exit 1)))
        (let* ((targ-link (car args))
               (link-name (cadr args))  
               (sub-path (string-reverse (string-join (cdr (string-split (string-reverse link-name) "/")) "/"))) 
               (msg         (or (args:get-arg "-m") "")))
               (if (> (string-length(string-trim sub-path)) 0)
                (begin 
                  (print "attempting to create directory " sub-path " in " target-dir)
                  (spublish:validate     target-dir sub-path)
                  (print (conc target-dir "/" sub-path ) )
                  (print (directory-exists?(conc target-dir "/" sub-path )))
                  (if (directory-exists?(conc target-dir "/" sub-path ))
                   (print "Target Directory " (conc target-dir sub-path ) " exist!!")
                  (spublish:mkdir configdat user target-dir sub-path msg))))

               (print "attempting to create link " link-name " in " target-dir)
               (spublish:ln configdat user target-dir targ-link link-name msg)))

      ((rm)
       (if (< (length args) 1)
	   (begin 
	     (print "ERROR: Missing arguments; " (string-intersperse args ", "))
	     (exit 1)))
       (let* ((targ-file (car args))
	      (msg         (or (args:get-arg "-m") "")))
	 (print "attempting to remove " targ-file " from " target-dir)
           (spublish:validate     target-dir targ-file)

	 (spublish:rm configdat user target-dir targ-file msg)))
      ((publish)
       (if (< (length args) 3)
	   (begin 
	     (print "ERROR: Missing arguments; " (string-intersperse args ", "))
	     (exit 1))
	   (let* ((srcpath  (list-ref args 0))
		  (areaname (list-ref args 1))
		  (version  (list-ref args 2))
		  (remargs  (args:get-args (drop args 2)
					   '("-type" ;; link or copy (default is copy)
					     "-m")
					   '()
					   args:arg-hash
					   0))
		  (publish-type (if (equal? (args:get-arg "-type") "link") 'link 'copy))
		  (comment      (or (args:get-arg "-m") ""))
		  (submitter    (current-user-name))
		  (quality      (args:get-arg "-quality"))
		  (publish-res  (spublish:publish configdat publish-type areaname version comment srcpath submitter quality)))
	     (if (not (car publish-res))
		 (begin
		   (print "ERROR: " (cdr publish-res))
		   (exit 1))))))
      ((list-versions)
       (let ((area-name (car args)) ;;      version patt   full print
	     (remargs   (args:get-args args '("-vpatt") '("-full") args:arg-hash 0))
	     (db        (spublish:open-db configdat))
	     (versions  (spublish:get-versions-for-area db (car args) version-patt: (args:get-arg "-vpatt"))))
	 ;; (print "area-name=" area-name " args=" args " *args-hash*=" (hash-table->alist *args-hash*))
	 (map (lambda (x)
		(if (args:get-arg "-full")
		    (format #t 
			    "~10a~10a~4a~27a~30a\n"
			    (vector-ref x 0)
			    (vector-ref x 1) 
			    (vector-ref x 2) 
			    (conc "\"" (time->string (seconds->local-time (vector-ref x 3))) "\"")
			    (conc "\"" (vector-ref x 4) "\""))
		    (print (vector-ref x 0))))
	      versions)))
      (else (print "Unrecognised command " action)))))
  
;; ease debugging by loading ~/.dashboardrc - REMOVE FROM PRODUCTION!
;; (let ((debugcontrolf (conc (get-environment-variable "HOME") "/.spublishrc")))
;;   (if (file-exists? debugcontrolf)
;;       (load debugcontrolf)))

(define (main)
  (let* ((args      (argv))
	 (prog      (car args))
	 (rema      (cdr args))
	 (exe-name  (pathname-file (car (argv))))
	 (exe-dir   (or (pathname-directory prog)
			(spublish:find exe-name (string-split (get-environment-variable "PATH") ":"))))
	 (configdat (spublish:load-config exe-dir exe-name)))
    (cond
     ;; one-word commands
     ((eq? (length rema) 1)
      (case (string->symbol (car rema))
	((help -h -help --h --help)
	 (print spublish:help))
	((list-vars) ;; print out the ini file
	 (map print (spublish:get-areas configdat)))
	((ls)
	 (let ((target-dir (configf:lookup configdat "settings" "target-dir")))
	   (print "Files in " target-dir)
	   (system (conc "ls " target-dir))))
	((log)
	 (spublish:db-do configdat (lambda (db)
				     (print "Listing actions")
				     (query (for-each-row
					     (lambda (row)
					       (apply print (intersperse row " | "))))
					    (sql db "SELECT * FROM actions")))))
	(else
	 (print "ERROR: Unrecognised command. Try \"spublish help\""))))
     ;; multi-word commands
     ((null? rema)(print spublish:help))
     ((>= (length rema) 2)
      (apply spublish:process-action configdat (car rema)(cdr rema)))
     (else (print "ERROR: Unrecognised command. Try \"spublish help\"")))))

(main)
