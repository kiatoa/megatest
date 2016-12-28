
;; Copyright 2006-2013, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

(use defstruct)
(use scsh-process)

(use refdb)


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
(use srfi-19)

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
;;; please create this file before using sautherise. For sample file is avaliable sample-sauth-paths.scm. 
(include "sauth-paths.scm")
(include "sauth-common.scm")


;;
;; GLOBALS
;;
(define *spublish:current-tab-number* 0)
(define *args-hash* (make-hash-table))
(define spublish:help (conc "Usage: spublish  [action [params ...]]

  ls       <area>              : list contents of target area
  cp|publish <area> <src file> <destination>      : copy file to target area
  mkdir <area> <dir name>       : maks directory in target area  
  rm <area> <file>              : remove file <file> from target area
  ln <area> <target> <link name> : creates a symlink
 
  options:

    -m \"message\"        : describe what was done
Note: All the target locations relative to base path 
Part of the Megatest tool suite.
Learn more at http://www.kiatoa.com/fossils/megatest

Version: " megatest-fossil-hash)) ;; "

;;======================================================================
;; RECORDS
;;======================================================================

;;======================================================================
;; DB
;;======================================================================

(define *default-log-port* (current-error-port))
(define *verbosity*         1)

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
	     (debug:print 2 *default-log-port* "ERROR: problem accessing db " dbpath
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
                            (print " ... file " targ-path " copied to " targ-path)
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

;; copy directory to dest, validation is done BEFORE calling this
;;

(define (spublish:tar configdat submitter target-dir dest-dir comment)
  (let ((dest-dir-path (conc target-dir "/" dest-dir)))
       (if (not(file-exists? dest-dir-path))
	(begin
	  (print "ERROR: target directory " dest-dir-path " does not exists." )
	  (exit 1)))
    ;;(print dest-dir-path )
    (spublish:db-do
     configdat
     (lambda (db)
       (spublish:register-action db "tar" submitter dest-dir-path comment)))
       (change-directory dest-dir-path)
       (process-wait (process-run "/bin/tar" (list "xf" "-")))
       (print "Data copied to " dest-dir-path) 

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
;;========================================================================
;;Shell 
;;========================================================================
(define (spublish:get-accessable-projects  area)
   (let* ((projects `()))
     ;  (print "in spublish:get-accessable-projects") 
        ;(print (spublish:has-permission area))
        (if (spublish:has-permission area)
               (set! projects (cons area projects))
               (begin
                 (print "User cannot access area " area "!!")  
                (exit 1))) 
       ;  (print "exiting spublish:get-accessable-projects")
    projects))

;; function to find sheets to which use has access 
(define (spublish:has-permission  area)
  ;(print "in spublish:has-permission")
  (let* ((username     (current-user-name))
        (ret-val #f))
  (cond
   ((equal? (is-admin username) #t)
     (set! ret-val #t))
    ((equal? (is-user "publish" username area) #t)
     (set! ret-val #t))
   ((equal? (is-user "writer-admin" username area) #t) 
     (set! ret-val #t))

   ((equal? (is-user "area-admin" username area) #t) 
     (set! ret-val #t))
   (else  
    (set! ret-val #f)))
  ;  (print ret-val)
     ret-val))

(define (is_directory target-path) 
  (let* ((retval #f))
  (sauthorize:do-as-calling-user
    	(lambda ()
          ;(print (current-effective-user-id) ) 
          (if (directory? target-path)
               (set! retval  #t))))
             ;(print (current-effective-user-id))
     retval)) 


(define (spublish:shell-cp src-path target-path)  
  (cond
   ((not (file-exists? target-path))
	(print "ERROR: target Directory " target-path " does not exist!!"))
   ((not (file-exists? src-path))
    (print "Error: Source path " src-path " does not exist!!" ))
   (else
     (if (is_directory src-path) 
        (begin
            (let* ((parent-dir src-path)
                   (start-dir target-path))
                 ;(print "parent-dir " parent-dir " start-dir " start-dir)   
                 (run (pipe
                   (begin (system (conc "cd " parent-dir " ;tar chf - ." )))
                   (begin (change-directory start-dir)
                          ;(print "123")
                          (run-cmd "tar" (list "xf" "-")))))
                          (print "Copied data to " start-dir))) 
        (begin
           (let*((parent-dir (pathname-directory src-path))
                  (start-dir target-path)
                (filename (if  (pathname-extension src-path)  
                                      (conc(pathname-file src-path) "." (pathname-extension src-path))
                                      (pathname-file src-path))))
                ;(print "parent-dir " parent-dir " start-dir " start-dir)   
                 (run (pipe
                   (begin (system (conc "cd " parent-dir ";tar chf - " filename )))
                   (begin (change-directory start-dir)
                          (run-cmd "tar" (list "xf" "-")))))
                          (print "Copied data to " start-dir)))))))


(define (spublish:shell-mkdir targ-path)
    (if (file-exists? targ-path)
	(begin
	  (print "ERROR: target Directory " targ-path " already exist!!"))
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
      (thread-join! th1)
    (cons #t "Successfully saved data"))))
 

(define (spublish:shell-rm targ-path)
    (if (not (file-exists? targ-path))
	(begin
	  (print "ERROR: target path " targ-path " does not exist!!"))
        (let* ((th1         (make-thread
			 (lambda ()
			   (delete-file  targ-path )
			   (print " ... path " targ-path " deleted"))
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
      (thread-join! th1)
    (cons #t "Successfully saved data"))))

(define (spublish:shell-ln src-path target-path sub-path)
   (if (not (file-exists? sub-path))
	 (print "ERROR: Path " sub-path " does not exist!! cannot proceed with link creation!!")
        (begin  
          (if (not (file-exists? src-path))
  	    (print "ERROR: Path " src-path " does not exist!! cannot proceed with link creation!!")
            (begin
                (if (file-exists? target-path)
                   (print "ERROR: Path " target-path "already exist!! cannot proceed with link creation!!")
                   (begin 
                      (create-symbolic-link src-path target-path  )
			   (print " ... link " target-path " created"))))))))
 
(define (spublish:shell-help)
(conc "Usage: [action [params ...]]

  ls    [target path]               	  : list contents of target area.
  cd    <target path> 	     	          : To change the current directory within the sretrive shell. 
  pwd				     	  : Prints the full pathname of the current directory within the sretrive shell.
  mkdir <path>                            : creates directory. Note it does not create's a path recursive manner.
  rm <target path>                        : removes files and emoty directories   
  cp <src> <target path>                  : copy a file/dir to target path. if src is a dir it automatically makes a recursive copy.
  ln TARGET LINK_NAME                     : creates a symlink      
Part of the Megatest tool suite.
Learn more at http://www.kiatoa.com/fossils/megatest

Version: " megatest-fossil-hash)
)	

(define (toplevel-command . args) #f)

(define (spublish:shell area)
 ; (print area)
  (use readline)
  (let* ((path      '())
	 (prompt    "spublish> ")
	 (args      (argv))
         (usr (current-user-name) )   
         (top-areas (spublish:get-accessable-projects area))
         (close-port     #f)
         (area-obj  (get-obj-by-code area))
         (user-obj (get-user usr)) 
         (base-path (if (null? area-obj) 
                         "" 
                        (caddr (cdr area-obj))))      
	 (iport     (make-readline-port prompt)))
        ;(print base-path) 
        (if (null? area-obj)
          (begin 
             (print "Area " area " does not exist")
          (exit 1)))
        ; (print "here")    
	(let loop ((inl (read-line iport)))
	  (if (not (or (or (eof-object? inl)
		       (equal? inl "exit")) (port-closed? iport)))
	      (let* ((parts (string-split inl))
		     (cmd   (if (null? parts) #f (car parts))))
		(if (and (not cmd) (not (port-closed? iport)))
		    (loop (read-line))
		    (case (string->symbol cmd)
		      ((cd)
		       (if (> (length parts) 1) ;; have a parameter
                           (begin
                             (let*((arg (cadr parts))
                                   (resolved-path (sauth-common:resolve-path  arg path top-areas))
                                   (target-path (sauth-common:get-target-path path  arg top-areas base-path)))
                                 (if (not (equal? target-path #f))
                                 (if (or (equal? resolved-path #f) (not (file-exists? target-path)))    
                                 (print "Invalid argument " arg ".. ")
                                  (begin      
			            (set! path resolved-path)
                                     (sauthorize:do-as-calling-user
                              (lambda ()
			    (run-cmd (conc *sauth-path* "/sauthorize") (list "register-log" (conc "\"" inl "\"") (number->string (car user-obj))  (number->string (caddr area-obj))  "cd"))))
                                  )))))  
   			   (set! path '())))
                      ((pwd)
                         (if (null? path)
                           (print "/")  
                           (print "/" (string-join path "/")))) 
		      ((ls)
		       (let* ((thepath (if (> (length parts) 1) ;; have a parameter
					   (cdr parts)
					   `()))
			      (plen    (length thepath)))
                         (cond
			  ((null? thepath)
                           (sauth-common:shell-ls-cmd path "" top-areas base-path  '())
                            (sauthorize:do-as-calling-user
                              (lambda ()
			    (run-cmd (conc *sauth-path* "/sauthorize") (list "register-log" (conc "\"" inl "\"") (number->string (car user-obj))  (number->string (caddr area-obj))  "ls"))))   )
			  ((< plen 2)
                            (sauth-common:shell-ls-cmd path  (car thepath) top-areas base-path '())
                              (sauthorize:do-as-calling-user
                              (lambda ()
			    (run-cmd (conc *sauth-path* "/sauthorize") (list "register-log" (conc "\"" inl "\"") (number->string (car user-obj))  (number->string (caddr area-obj))  "ls")))))
                          (else 
                            (if (equal? (car thepath) "|")
                              (sauth-common:shell-ls-cmd path "" top-areas base-path thepath)
                              (sauth-common:shell-ls-cmd path  (car thepath) top-areas base-path (cdr thepath)))
                           (sauthorize:do-as-calling-user
                              (lambda ()
			    (run-cmd (conc *sauth-path* "/sauthorize") (list "register-log" (conc "\"" inl "\"") (number->string (car user-obj))  (number->string (caddr area-obj))  "ls"))))))))
                       ((mkdir)
                         (let* ((thepath (if (> (length parts) 1) ;; have a parameter
				   (cdr parts)
				   `()))
			      (plen    (length thepath)))
                         (cond
                          ((null? thepath)
                            (print "mkdir takes one argument"))
                          ((< plen 2) 
                            (let*((mk-path (cadr parts))
                                  (resolved-path (sauth-common:resolve-path  mk-path path top-areas))
                                  (target-path (sauth-common:get-target-path path  mk-path top-areas base-path)))
                              (if (not (equal? target-path #f))
                                 (if (equal? resolved-path #f)     
                                 (print "Invalid argument " mk-path ".. ")
                                  (begin 
                                      (spublish:shell-mkdir target-path)   
                                      (sauthorize:do-as-calling-user
                              (lambda ()
			    (run-cmd (conc *sauth-path* "/sauthorize") (list "register-log" (conc "\"" inl "\"") (number->string (car user-obj))  (number->string (caddr area-obj))  "mkdir")))))))
		       )))))
                       ((rm)
                          (let* ((thepath (if (> (length parts) 1) ;; have a parameter
				   (cdr parts)
				   `()))
			      (plen    (length thepath)))
                         (cond
                          ((null? thepath)
                            (print "rm takes one argument"))
                          ((< plen 2) 
                            (let*((rm-path (cadr parts))
                                  (resolved-path (sauth-common:resolve-path  rm-path path top-areas))
                                  (target-path (sauth-common:get-target-path path  rm-path top-areas base-path)))
                              (if (not (equal? target-path #f))
                                 (if (equal? resolved-path #f)     
                                 (print "Invalid argument " rm-path ".. ")
                                  (begin 
                                      (spublish:shell-rm target-path)   
                                      (sauthorize:do-as-calling-user
                              (lambda ()
			    (run-cmd (conc *sauth-path* "/sauthorize") (list "register-log" (conc "\"" inl "\"") (number->string (car user-obj))  (number->string (caddr area-obj))  "rm")))))))
		       )))))

                      ((cp publish)
                          (let* ((thepath (if (> (length parts) 1) ;; have a parameter
				   (cdr parts)
				   `()))
			      (plen    (length thepath)))
                         (cond
                          ((or (null? thepath) (< plen 2)) 
                            (print "cp takes two argument"))
                          ((< plen 3) 
                            (let*((src-path (car thepath))
                                  (dest-path (cadr thepath))   
                                  (resolved-path (sauth-common:resolve-path  dest-path path top-areas))
                                  (target-path (sauth-common:get-target-path path  dest-path top-areas base-path)))
                              (if (not (equal? target-path #f))
                                 (if (equal? resolved-path #f)     
                                 (print "Invalid argument " dest-path ".. ")
                                  (begin 
                                      (spublish:shell-cp src-path target-path)   
                                      (sauthorize:do-as-calling-user
                              (lambda ()
			    (run-cmd (conc *sauth-path* "/sauthorize") (list "register-log" (conc "\"" inl "\"") (number->string (car user-obj))  (number->string (caddr area-obj))  "cp")))))))
		       )))))
                      ((ln)
                           (let* ((thepath (if (> (length parts) 1) ;; have a parameter
				   (cdr parts)
				   `()))
			      (plen    (length thepath)))
                         (cond
                          ((or (null? thepath) (< plen 2)) 
                            (print "ln takes two argument"))
                          ((< plen 3) 
                            (let*((src-path (car thepath))
                                  (dest-path (cadr thepath))   
                                  (resolved-path (sauth-common:resolve-path  dest-path path top-areas))
                                  (target-path (sauth-common:get-target-path path  dest-path top-areas base-path))
                                  (sub-path (conc "/" (string-reverse (string-join (cdr (string-split (string-reverse  target-path) "/")) "/")))))
                              (if (not (equal? target-path #f))
                                 (if (equal? resolved-path #f)     
                                 (print "Invalid argument " dest-path ".. ")
                                  (begin 
                                      (spublish:shell-ln src-path target-path sub-path)   
                                      (sauthorize:do-as-calling-user
                              (lambda ()
			    (run-cmd (conc *sauth-path* "/sauthorize") (list "register-log" (conc "\"" inl "\"") (number->string (car user-obj))  (number->string (caddr area-obj))  "ln")))))))
		       )))))  
                      ((exit)
                          (print "got exit"))  
                      ((help)
                          (print (spublish:shell-help)))
		      (else 
		       (print "Got command: " inl))))
                 (loop (read-line iport)))))))


;;======================================================================
;; MAIN
;;======================================================================

(define (spublish:load-config exe-dir exe-name)
  (let* ((fname   (conc exe-dir "/." exe-name ".config")))
    ;; (ini:property-separator-patt " *  *")
    ;; (ini:property-separator #\space)
    (if (file-exists? fname)
	;; (ini:read-ini fname)
	(read-config fname #f #t)
	(make-hash-table))))

(define (spublish:process-action action . args)
  ;(print args)
  (let* ((usr          (current-user-name))
         (user-obj (get-user usr)) 
         (area   (car args))
         (area-obj  (get-obj-by-code area))
         (top-areas (spublish:get-accessable-projects area))  
         (base-path (if (null? area-obj) 
                         "" 
                        (caddr (cdr area-obj))))   
         (remargs (cdr args)))
     (if (null? area-obj)
          (begin 
             (print "Area " area " does not exist")
          (exit 1)))
    (case (string->symbol action)
      ((cp publish)
       (if (< (length remargs) 2)
	   (begin 
	     (print "ERROR: Missing arguments; spublish <area> <src file> <destination>" )
	     (exit 1)))
       (let* ((filter-args     (args:get-args args '("-m") '() args:arg-hash 0))
              (src-path-in (car filter-args))
              (dest-path (cadr filter-args))
	      (src-path    (with-input-from-pipe
			    (conc "readlink -f " src-path-in)
			    (lambda ()
			      (read-line))))
	      (msg         (or (args:get-arg "-m") ""))
              (resolved-path (sauth-common:resolve-path  (conc area "/" dest-path) `() top-areas))
              (target-path (sauth-common:get-target-path `()  (conc area "/" dest-path) top-areas base-path)))
 	     (if (not (equal? target-path #f))
                 (if (equal? resolved-path #f)     
                    (print "Invalid argument " dest-path ".. ")
                    (begin 
                      (spublish:shell-cp src-path target-path)   
                      (sauthorize:do-as-calling-user
                      (lambda ()
		        (run-cmd (conc *sauth-path* "/sauthorize") (list "register-log" (conc "\" cp " src-path-in " " dest-path  "\"") (number->string (car user-obj))  (number->string (caddr area-obj))  "cp")))))))))   
      ((mkdir)
        (if (< (length remargs) 1)
          (begin 
	     (print "ERROR: Missing arguments; <area> <path>")
	     (exit 1)))
        (let* ((filter-args     (args:get-args args '("-m") '() args:arg-hash 0))
               (mk-path (car filter-args))
               (msg         (or (args:get-arg "-m") ""))
               (resolved-path (sauth-common:resolve-path  mk-path (list area) top-areas))
               (target-path (sauth-common:get-target-path (list area)  mk-path top-areas base-path))) 
               (print "attempting to create directory " mk-path  )
               (if (not (equal? target-path #f))
                 (if (equal? resolved-path #f)     
                   (print "Invalid argument " mk-path ".. ")
                   (begin 
                     (spublish:shell-mkdir target-path)   
                     (sauthorize:do-as-calling-user
                       (lambda ()
		        (run-cmd (conc *sauth-path* "/sauthorize") (list "register-log" (conc "\" mkdir " mk-path  "\"") (number->string (car user-obj))  (number->string (caddr area-obj))  "mkdir")))))))))  
      ((ln) 
        (if (< (length remargs) 2)
          (begin 
	     (print "ERROR: Missing arguments;  <area> <target> <link name>" )
	     (exit 1)))
        (let* ((filter-args     (args:get-args args '("-m") '() args:arg-hash 0))
              (src-path (car filter-args))
              (dest-path (cadr filter-args))   
              (resolved-path (sauth-common:resolve-path  dest-path (list area) top-areas))
              (target-path (sauth-common:get-target-path  (list area)  dest-path top-areas base-path))
              (sub-path (conc "/" (string-reverse (string-join (cdr (string-split (string-reverse  target-path) "/")) "/")))))
               (if (not (equal? target-path #f))
                  (if (equal? resolved-path #f)     
                    (print "Invalid argument " dest-path ".. ")
                    (begin 
                   (spublish:shell-ln src-path target-path sub-path)   
                  (sauthorize:do-as-calling-user
                 (lambda ()
		   (run-cmd (conc *sauth-path* "/sauthorize") (list "register-log" (conc "\" ln " src-path " " dest-path  "\"") (number->string (car user-obj))  (number->string (caddr area-obj))  "ln")))))))))
      ((rm)
       (if (< (length remargs) 1)
	   (begin 
	     (print "ERROR: Missing arguments; <area> <path> ")
	     (exit 1)))
       (let* ((filter-args  (args:get-args args '("-m") '() args:arg-hash 0))
              (rm-path (car filter-args))
              (resolved-path (sauth-common:resolve-path  rm-path (list area) top-areas))
              (target-path (sauth-common:get-target-path (list area)  rm-path top-areas base-path)))
	       (if (not (equal? target-path #f))
                                 (if (equal? resolved-path #f)     
                                 (print "Invalid argument " rm-path ".. ")
                                  (begin 
                                      (spublish:shell-rm target-path)   
                                      (sauthorize:do-as-calling-user
                              (lambda ()
			    (run-cmd (conc *sauth-path* "/sauthorize") (list "register-log" (conc "\" rm " rm-path "\"") (number->string (car user-obj))  (number->string (caddr area-obj))  "rm")))))))))
      ((shell)
          (if (< (length args) 1)
             (begin 
	     (print  "ERROR: Missing arguments area!!" )
	     (exit 1))
             (spublish:shell area))) 
      (else (print "Unrecognised command " action)))))
  
;; ease debugging by loading ~/.dashboardrc - REMOVE FROM PRODUCTION!
;; (let ((debugcontrolf (conc (get-environment-variable "HOME") "/.spublishrc")))
;;   (if (file-exists? debugcontrolf)
;;       (load debugcontrolf)))

(define (main)
  (let* ((args      (argv))
	 (prog      (car args))
	 (rema      (cdr args))
	 (exe-name  (pathname-file (car (argv)))))
    (cond
     ;; one-word commands
     ((eq? (length rema) 1)
      (case (string->symbol (car rema))
	((help -h -help --h --help)
	 (print spublish:help))
	(else
	 (print "ERROR: Unrecognised command. Try \"spublish help\""))))
     ;; multi-word commands
     ((null? rema)(print spublish:help))
     ((>= (length rema) 2)
      (apply spublish:process-action (car rema)(cdr rema)))
     (else (print "ERROR: Unrecognised command2. Try \"spublish help\"")))))

(main)
