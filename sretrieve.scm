
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
(use srfi-18)
(use srfi-19)
(use refdb)
(use sql-de-lite srfi-1 posix regex regex-case srfi-69)
(declare (uses common))
(declare (uses configf))
(declare (uses margs))
(declare (uses megatest-version))
 

(include "megatest-fossil-hash.scm")
;;; please create this file before using sautherise. For sample file is avaliable sample-sauth-paths.scm. 
(include "sauth-paths.scm")
(include "sauth-common.scm")

(define (toplevel-command . args) #f)
(use readline)


;;
;; GLOBALS
;;
(define *verbosity* 1)
(define *logging* #f)
(define *exe-name* (pathname-file (car (argv))))
(define *sretrieve:current-tab-number* 0)
(define *args-hash* (make-hash-table))
(define sretrieve:help (conc "Usage: " *exe-name* " [action [params ...]]

  ls   <area>                        : list contents of target area
  get  <area>  <reletive path>       : retrieve path to the data within <area>
     -m \"message\"       : why retrieved?
  shell  <area>                   : start a shell-like interface

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
;(define (sretrieve:initialize-db db)
;  (for-each
;   (lambda (qry)
;     (exec (sql db qry)))
;   (list 
;    "CREATE TABLE IF NOT EXISTS actions
;         (id           INTEGER PRIMARY KEY,
;          action       TEXT NOT NULL,
;          retriever    TEXT NOT NULL,
;          datetime     TIMESTAMP DEFAULT (datetime('now','localtime')),
;          srcpath      TEXT NOT NULL,
;          comment      TEXT DEFAULT '' NOT NULL,
;          state        TEXT DEFAULT 'new');"
;    "CREATE TABLE IF NOT EXISTS bundles
;         (id           INTEGER PRIMARY KEY,
;          bundle       TEXT NOT NULL,
;          release      TEXT NOT NULL,
;          status       TEXT NOT NULL,
;          event_date   TEXT NOT NULL);"
;    )))
;
;(define (sretrieve:register-action db action submitter source-path comment)
; ; (print "(sretrieve:register-action db " db " action " action " submitter " submitter " source-path " source-path " comment " comment)
;  (exec (sql db "INSERT INTO actions (action,retriever,srcpath,comment)
;                 VALUES(?,?,?,?)")
;	action
;	submitter
;	source-path
;	(or comment "")))

;; (call-with-database
;;  (lambda (db)
;;   (set-busy-handler! db (busy-timeout 10000)) ; 10 second timeout
;;   ...))

;; Create the sqlite db
;(define (sretrieve:db-do configdat proc) 
;  (let ((path (configf:lookup configdat "database" "location")))
;    (if (not path)
;	(begin
;	  (debug:print 0 "[database]\nlocation /some/path\n\n Is missing from the config file!")
;	  (exit 1)))
;    (if (and path
;	     (directory? path)
;	     (file-read-access? path))
;	(let* ((dbpath    (conc path "/" *exe-name* ".db"))
;	       (writeable (file-write-access? dbpath))
;	       (dbexists  (file-exists? dbpath)))
;	  (handle-exceptions
;	   exn
;	   (begin
;	     (debug:print 2 "ERROR: problem accessing db " dbpath
;			  ((condition-property-accessor 'exn 'message) exn))
;	     (exit 1))
;            ;;(debug:print 0 "calling proc " proc "db path " dbpath )
;	   (call-with-database
;            dbpath
;	    (lambda (db)
;	       ;;(debug:print 0 "calling proc " proc " on db " db)
;	      (set-busy-handler! db (busy-timeout 10000)) ;; 10 sec timeout
;	      (if (not dbexists)(sretrieve:initialize-db db))
;	      (proc db)))))
;	(debug:print 0 "ERROR: invalid path for storing database: " path))))

;; copy in directory to dest, validation is done BEFORE calling this
;;
;(define (sretrieve:get configdat retriever version comment)
;  (let* ((base-dir  (configf:lookup configdat "settings" "base-dir"))
;	 (datadir   (conc base-dir "/" version)))
;    (if (or (not base-dir)
;	    (not (file-exists? base-dir)))
;	(begin
;	  (debug:print 0 "ERROR: Bad configuration! base-dir " base-dir " not found")
;	  (exit 1)))
;    (print datadir)
;    (if (not (file-exists? datadir))
;	(begin
;	  (debug:print 0 "ERROR: Bad version (" version "), no data found at " datadir "." )
;	  (exit 1)))
;    
;    (sretrieve:db-do
;     configdat
;     (lambda (db)
;       (sretrieve:register-action db "get" retriever datadir comment)))
;      (sretrieve:do-as-calling-user
;       (lambda ()
;         (if (directory? datadir)
;	   (begin
;  	    (change-directory datadir)
;	    (let ((files (filter (lambda (x)
;				(not (member x '("." ".."))))
;			      (glob "*" ".*"))))
;	     (print "files: " files)
;	     (process-execute "/bin/tar" (append (append (list  "chfv" "-") files) (list "--ignore-failed-read")))))
;             (begin
;               (let* ((parent-dir (pathname-directory datadir) )
;                      (filename  (conc(pathname-file datadir) "." (pathname-extension datadir))))
;                  (change-directory parent-dir)  
;                  (process-execute "/bin/tar" (list "chfv" "-" filename))
;             )))
;))))
;
;
;;; copy in file to dest, validation is done BEFORE calling this
;;;
;(define (sretrieve:cp configdat retriever file comment)
;  (let* ((base-dir  (configf:lookup configdat "settings" "base-dir"))
;         (allowed-sub-paths (configf:lookup configdat "settings" "allowed-sub-paths"))    
;	 (datadir   (conc base-dir "/" file))
;         (filename  (conc(pathname-file datadir) "." (pathname-extension datadir))))
;    (if (or (not base-dir)
;	    (not (file-exists? base-dir)))
;	(begin
;	  (debug:print 0 "ERROR: Bad configuration! base-dir " base-dir " not found")
;	  (exit 1)))
;    (print datadir)
;    (if (not (file-exists? datadir))
;	(begin
;	  (debug:print 0 "ERROR: File  (" file "), not found at " base-dir "." )
;	  (exit 1)))
;    (if (directory? datadir)
;	(begin
;	  (debug:print 0 "ERROR: (" file ") is a dirctory!! cp cmd works only on files ." )
;	  (exit 1)))
;    (if(not (string-match (regexp  allowed-sub-paths) file))
;        (begin
;	  (debug:print 0 "ERROR: Access denied to file (" file ")!! " )
;	  (exit 1)))
;     
;     (sretrieve:db-do
;     configdat
;     (lambda (db)
;       (sretrieve:register-action db "cp" retriever datadir comment)))
;      (sretrieve:do-as-calling-user
;      ;;  (debug:print 0 "ph:  "(pathname-directory datadir)  "!! " )
;       (change-directory (pathname-directory datadir))  
;       ;;(debug:print 0 "ph: /bin/tar" (list "chfv" "-" filename) )
;      (process-execute "/bin/tar" (list "chfv" "-" filename)))
;      ))
;
;;; ls in file to dest, validation is done BEFORE calling this
;;;
;(define (sretrieve:ls configdat retriever file comment)
;  (let* ((base-dir  (configf:lookup configdat "settings" "base-dir"))
;         (allowed-sub-paths (configf:lookup configdat "settings" "allowed-sub-paths"))    
;	 (datadir   (conc base-dir "/" file))
;         (filename  (conc(pathname-file datadir) "." (pathname-extension datadir))))
;    (if (or (not base-dir)
;	    (not (file-exists? base-dir)))
;	(begin
;	  (debug:print 0 "ERROR: Bad configuration! base-dir " base-dir " not found")
;	  (exit 1)))
;    (print datadir)
;    (if (not (file-exists? datadir))
;	(begin
;	  (debug:print 0 "ERROR: File  (" file "), not found at " base-dir "." )
;	  (exit 1)))
;      (if(not (string-match (regexp  allowed-sub-paths) file))
;        (begin
;	  (debug:print 0 "ERROR: Access denied to file (" file ")!! " )
;	  (exit 1)))
;   
;        (sretrieve:do-as-calling-user
;        (lambda ()
;	  (process-execute "/bin/ls" (list "-ls"  "-lrt" datadir ))
; ))))



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


;(define (sretrieve:backup-move path)
;  (let* ((trashdir  (conc (pathname-directory path) "/.trash"))
;	 (trashfile (conc trashdir "/" (current-seconds) "-" (pathname-file path))))
;    (create-directory trashdir #t)
;    (if (directory? path)
;	(system (conc "mv " path " " trashfile))
;	(file-move path trash-file))))
;
;
;(define (sretrieve:lst->path pathlst)
;  (conc "/" (string-intersperse (map conc pathlst) "/")))
;
;(define (sretrieve:path->lst path)
;  (string-split path "/"))
;
;(define (sretrieve:pathdat-apply-heuristics configdat path)
;  (cond
;   ((file-exists? path) "found")
;   (else (conc path " not installed"))))

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
;; SHELL
;;======================================================================

;; Create the sqlite db for shell
;(define (sretrieve:shell-db-do path proc) 
;    (if (not path)
;	(begin
;	  (debug:print 0 "[database]\nlocation /some/path\n\n Is missing from the config file!")
;	  (exit 1)))
;    (if (and path
;	     (directory? path)
;	     (file-read-access? path))
;	(let* ((dbpath    (conc path "/" *exe-name* ".db"))
;	       (writeable (file-write-access? dbpath))
;	       (dbexists  (file-exists? dbpath)))
;	  (handle-exceptions
;	   exn
;	   (begin
;	     (debug:print 2 "ERROR: problem accessing db " dbpath
;			  ((condition-property-accessor 'exn 'message) exn))
;	     (exit 1))
;            ;;(debug:print 0 "calling proc " proc "db path " dbpath )
;	   (call-with-database
;            dbpath
;	    (lambda (db)
;	       ;;(debug:print 0 "calling proc " proc " on db " db)
;	      (set-busy-handler! db (busy-timeout 10000)) ;; 10 sec timeout
;	      (if (not dbexists)(sretrieve:initialize-db db))
;	      (proc db)))))
;	(debug:print 0 "ERROR: invalid path for storing database: " path)))



;; function to find sheets to which use has access 
(define (sretrieve:has-permission  area)
  (let ((username     (current-user-name)))
  (cond
   ((is-admin username)
     #t)
   ((is-user "retrieve" username area) 
     #t)
    ((is-user "publish" username area) 
     #t)
   ((is-user "writer-admin" username area) 
     #t)
   ((is-user "read-admin" username area) 
     #t)
   ((is-user "area-admin" username area) 
     #t)
   (else  
    #f))))
 

(define (sretrieve:get-accessable-projects  area)
   (let* ((projects `()))
         
        (if (sretrieve:has-permission area)
               (set! projects (cons area projects))
               (begin
                 (print "User cannot access area " area "!!")  
                (exit 1))) 
   ; (print projects)
    projects))
		 
(define (sauth-common:shell-ls-cmd base-path-list ext-path top-areas base-path tail-cmd-list)
    (if (and (null? base-path-list) (equal? ext-path "") )
      (print (string-intersperse top-areas " "))
  (let* ((resolved-path (sauth-common:resolve-path ext-path base-path-list top-areas )))
           ;(print resolved-path)
           (if (not (equal? resolved-path #f))
           (if (null? resolved-path) 
             (print (string-intersperse top-areas " "))
           (let* ((target-path (sauth-common:get-target-path  base-path-list  ext-path top-areas base-path)))
                ;(print "Resolved path: " target-path)
                (if (not (equal? target-path #f))
                  (begin   
                (if (symbolic-link? target-path)
                   (set! target-path (conc target-path "/"))) 
                (if (not (equal? target-path #f))
                (begin 
                (cond
		  ((null? tail-cmd-list)
		     (run (pipe
      	      	      (ls "-lrt" ,target-path))))
		  ((not (equal? (car tail-cmd-list) "|"))
                         (print "ls cmd cannot accept "(string-join tail-cmd-list) " as an argument!!"))
                  (else  
                    (run (pipe
      	      	      (ls "-lrt" ,target-path)
                      (begin (system (string-join (cdr tail-cmd-list))))))
      ))))))))))))

(define (sretrieve:shell-cat-cmd base-pathlist ext-path top-areas base-path tail-cmd-list)
  (let* ((resolved-path (sauth-common:resolve-path ext-path base-pathlist top-areas ))
          (data "") )
         (if (not (equal? resolved-path #f))
           (if (null? resolved-path) 
             (print "Path could not be resolved!!")
           (let* ((target-path (sauth-common:get-target-path  base-pathlist  ext-path top-areas base-path)))
             (if (not (equal? target-path #f))
               (if (or (not (file-exists? target-path)) (directory? target-path))
               (print "Target path does not exist or is a directory!") 
               (begin 
                 (cond
		  ((null? tail-cmd-list)
		     (run (pipe
      	      	      (cat  ,target-path))))
		  ((not (equal? (car tail-cmd-list) "|"))
                         (print "cat cmd cannot accept "(string-join tail-cmd-list) " as an argument!!"))
                  (else  
                    (run (pipe
      	      	      (cat ,target-path)
                      (begin (system (string-join (cdr tail-cmd-list))))))))))
)))
             (print "Path could not be resolved!!"))))

(define (get-options cmd-list split-str)
    (if (null? cmd-list)
              (list '() '())
    (let loop ((hed (car cmd-list))
		 (tal (cdr cmd-list))
                  (res '()))
                   (cond
                   ((equal? hed split-str)
                      (list res tal))
                   ((null? tal)
                      (list (cons hed res) tal)) 
                   (else 
		  	(loop (car tal)(cdr tal)(cons hed res)))))))

  
(define (sretrieve:shell-grep-cmd base-pathlist ext-path top-areas base-path tail-cmd-list)
  (let* ((resolved-path (sauth-common:resolve-path ext-path base-pathlist top-areas ))
          (pattern (car tail-cmd-list))
          (pipe-cmd-list (get-options (cdr tail-cmd-list) "|"))
          (options (string-join (car pipe-cmd-list)))
          (pipe-cmd (cadr  pipe-cmd-list))
          (redirect-split (string-split (string-join  tail-cmd-list) ">"))   )
         (if(and ( > (length redirect-split) 2 ))
            (print "sgrep  cmd cannot accept > " (string-join redirect-split) " as an argument!!" )  
         (if (not (equal? resolved-path #f))
           (if (null? resolved-path) 
             (print "Path could not be resolved!!")
           (let* ((target-path (sauth-common:get-target-path  base-pathlist  ext-path top-areas base-path))
                  (restrictions (if (equal? target-path #f)
                                                 ""
                                               (sretrieve:shell-lookup base-path)))
                 (rest-str  (string-split (conc " --exclude-dir=" (string-join (string-split restrictions ",") " --exclude-dir=") ))))
             (if (not (file-exists? target-path))
               (print "Target path does not exist!") 
               (begin 
                 (cond
	 	  ((and (null? pipe-cmd) (string-null? options))
	 	     (run (pipe
      	      	      (grep ,pattern ,target-path ))))
		  ((and (null? pipe-cmd) (not (string-null? options)))
		     (run (pipe
      	      	      (begin (process-execute "/usr/bin/grep" (append (list options pattern target-path) rest-str))))))
                  ((and (not (null? pipe-cmd)) (string-null? options))
                        (run (pipe
      	      	      (grep ,exclude-dir ,pattern ,target-path)
                      (begin (system (string-join pipe-cmd))))))
                  (else   
                      (run (pipe
      	      	      ;(grep  ,options ,exclude-dir ,pattern ,target-path)
                       (begin (process-execute "/usr/bin/grep" (append (list options pattern target-path) rest-str)))

                      (begin (system (string-join  pipe-cmd)))))))
))))
             (print "Path could not be resolved!!")))))


(define (sretrieve:shell-less-cmd base-pathlist ext-path top-areas base-path)
  (let* ((resolved-path (sauth-common:resolve-path ext-path base-pathlist top-areas )))
         (if (not (equal? resolved-path #f))
           (if (null? resolved-path) 
             (print "Path could not be resolved!!")
           (let* ((target-path (sauth-common:get-target-path  base-pathlist  ext-path top-areas base-path)))
               (if (not (equal? target-path #f))
           	 (if (or (not (file-exists? target-path)) (directory? target-path))
                   (print "Target path does not exist or is a directory!") 
                   (begin 
                     ;(sretrieve:shell-db-do
         	     ;     db-location
     		     ;	  (lambda (db)
       		;		(sretrieve:register-action db "less" (current-user-name) target-path (conc "Executing cmd: less "  target-path))))   
 
                     (setenv "LESSSECURE" "1")
                     (run (pipe
      	      	     (less ,target-path))))))))
             (print "Path could not be resolved!!"))))



(define (sretrieve:shell-lookup base-path)
  (let* ((usr (current-user-name))
          (value (get-restrictions base-path usr)))
                              value))
             

(define (sretrieve:load-shell-config fname)
          (if (file-exists? fname)
	(read-config fname #f #f)
	))


(define (is_directory target-path) 
  (let* ((retval #f))
  (sretrieve:do-as-calling-user
    	(lambda ()
          ;(print (current-effective-user-id) ) 
          (if (directory? target-path)
               (set! retval  #t))))
             ;(print (current-effective-user-id))
     retval)) 

(define (make-exclude-pattern  restriction-list  )
  (if (null? restriction-list)
     ""
   (let loop ((hed (car restriction-list))
		 (tal (cdr restriction-list))
                  (ret-str ""))
                   (cond
                   ((null? tal)
                      (conc ret-str ".+" hed ".*")) 
                   (else 
		  	(loop (car tal)(cdr tal)(conc ret-str ".+" hed ".*|"))))))    )

(define (sretrieve:get-shell-cmd target-path base-path restrictions iport)
     (if (not (file-exists? target-path))
        (print "Target path does not exist!")
    (begin
    (if (not (equal? target-path #f))
    (begin     
        (if (is_directory target-path) 
        (begin
           (let* ((tmpfile (conc "/tmp/" (current-user-name) "/my-pipe"))
                  (parent-dir target-path)
                  (last-dir-name (if  (pathname-extension target-path)  
                                      (conc(pathname-file target-path) "." (pathname-extension target-path))
                                      (pathname-file target-path)))
                  (curr-dir (current-directory))   
                  (start-dir (conc (current-directory) "/" last-dir-name))
                  (execlude (make-exclude-pattern (string-split restrictions ","))))
                  ; (print tmpfile)
                    (if  (file-exists? start-dir)
                    (begin
                         (print last-dir-name " already exist in your work dir. Do you want to over write it? [y|n]")
                        (let* ((inl (read-line iport)))
                            (if (equal? inl "y")
                              (begin
                                 (change-directory parent-dir)
                                  (create-fifo  tmpfile)
                                  (process-fork 
    				   (lambda()
                                       (sleep 1) 
       					(with-output-to-file tmpfile
         				(lambda ()
            				(sretrieve:make_file parent-dir execlude parent-dir)))))
 
                                  (run (pipe
                   		   (tar "chfv" "-" "-T" ,tmpfile )
                   		   (begin (system (conc "cd " start-dir ";tar  xUf - "   )))))
                                   (change-directory curr-dir)
                                    (system (conc "rm " tmpfile)) )
			      (begin	
                               (print  "Nothing has been retrieved!!  ")))))
                     (begin
                       (sretrieve:do-as-calling-user
                    (lambda ()
		      (create-directory start-dir #t)))
                          (change-directory parent-dir)
                          ; (print execlude)
                           (create-fifo tmpfile)
                            (process-fork 
    				   (lambda()
                                       (sleep 1) 
       					(with-output-to-file tmpfile
         				(lambda ()
            				(sretrieve:make_file parent-dir execlude parent-dir)))))

                                  (run (pipe
                   		   (tar "chfv" "-"  "-T" ,tmpfile)
                   		   (begin (system (conc "cd " start-dir ";tar  xUf - "    )))))
                           (change-directory curr-dir)
                            (system (conc "rm " tmpfile)))))) 
        (begin
           (let*((parent-dir (pathname-directory target-path))
                 (start-dir (current-directory))
                 (filename (if  (pathname-extension target-path)  
                                      (conc(pathname-file target-path) "." (pathname-extension target-path))
                                      (pathname-file target-path)))
                 (work-dir-file (conc (current-directory) "/" filename)))
                 (if  (file-exists? work-dir-file)
                    (begin
                       (print filename " already exist in your work dir. Do you want to over write it? [y|n]")
                        (let* ((inl (read-line iport)))
                            (if (equal? inl "y")
                              (begin
                                 (change-directory parent-dir)
                                  (run (pipe
                   		   (tar "chfv" "-" ,filename)
                   		   (begin (system (conc "cd " start-dir ";tar  xUf - "   )))))
                                     (change-directory start-dir))
			      (begin	
                               (print  "Nothing has been retrieved!!  ")))))
                    (begin
               (change-directory parent-dir)  
                 (run (pipe
                   (tar "chfv" "-" ,filename)
                   (begin (system (conc "cd " start-dir ";tar xUf -")))))
                    (change-directory start-dir)))))))))))

(define (sretrieve:get-shell-cmd-line target-path base-path restrictions iport)
   (handle-exceptions
	   exn 
	   (begin
	     (sauth:print-error (conc "Problem fetching the data. Sauth provieds sudo access to only one unix group. Please ensure you have washed all the remaining groups. System Error: " 
			  ((condition-property-accessor 'exn 'message) exn)))
	     (exit 1))
  
    (if (not (file-exists? target-path))
        (print "Error:Target path does not exist!")
    (begin
    (if (not (equal? target-path #f))
    (begin     
        (if (is_directory target-path) 
        (begin
           (let* ((parent-dir target-path)
                  (last-dir-name (if  (pathname-extension target-path)  
                                      (conc(pathname-file target-path) "." (pathname-extension target-path))
                                      (pathname-file target-path)))
                  (curr-dir (current-directory))   
                  (start-dir (conc (current-directory) "/" last-dir-name))
                  (execlude (make-exclude-pattern (string-split restrictions ",")))
                   (tmpfile (conc "/tmp/" (current-user-name) "/my-pipe-" (current-process-id))))
                    (if  (file-exists? start-dir)
                    (begin
                         (print last-dir-name " already exist in your work dir.")
                         (print  "Nothing has been retrieved!!  "))
                     (begin
                   ;    (sretrieve:do-as-calling-user
                   ; (lambda ()
                    
                  (if (not (file-exists?  (conc "/tmp/" (current-user-name)))) 
		      (create-directory (conc "/tmp/" (current-user-name)) #t))
                          (change-directory parent-dir)
                            (create-fifo  tmpfile)
                                  (process-fork 
    				   (lambda()
                                       (sleep 1) 
       					(with-output-to-file tmpfile
         				(lambda ()
            				(sretrieve:make_file parent-dir execlude parent-dir)))))

                           (process-execute "/bin/tar" (append (list  "chfv" "-"  "-T" tmpfile)  (list "--ignore-failed-read")))    
                                  ;(run (pipe
                   		   ;(tar "chfv" "-" "." )
                   		   ;(begin (system (conc "cd " start-dir ";tar  xUf - "   execlude )))))
                            (system (conc "rm " tmpfile))    
                           (change-directory curr-dir))))) 
        (begin
           (let*((parent-dir (pathname-directory target-path))
                 (start-dir (current-directory))
                 (filename (if  (pathname-extension target-path)  
                                      (conc(pathname-file target-path) "." (pathname-extension target-path))
                                      (pathname-file target-path)))
                 (work-dir-file (conc (current-directory) "/" filename)))
                 (if  (file-exists? work-dir-file)
                    (begin
                       (print filename " already exist in your work dir.")
                               (print  "Nothing has been retrieved!!  "))
                    (begin
               (change-directory parent-dir)
                (process-execute "/bin/tar" (append (append (list  "chfv" "-") (list filename)) (list "--ignore-failed-read"))) 
                 ;(run (pipe
                  ; (tar "chfv" "-" ,filename)
                  ; (begin (system (conc "cd " start-dir ";tar xUf -")))))
                    (change-directory start-dir))))))))))))

(define (sretrieve:make_file path exclude base_path)
   (find-files 
     path
     action: (lambda (p res)
           (cond
                ((symbolic-link? p)   
                 (if (directory?(read-symbolic-link p)) 
                      (sretrieve:make_file p exclude base_path)
                      (print (string-substitute (conc base_path "/") "" p "-"))))
                 ((directory? p)              
                 ;;do nothing for dirs)
                 ) 
                (else 
                                        
                     (if (not (string-match (regexp exclude)  p ))
                        (print (string-substitute (conc base_path "/") "" p "-"))))))))

(define (sretrieve:shell-help)
(conc "Usage: " *exe-name* " [action [params ...]]

  ls    [target path]               	  : list contents of target area. The output of the cmd can be piped into other system cmd. eg ls <path> | grep txt
  cd    <target directory>	     	  : To change the current directory within the sretrive shell. 
  pwd				     	  : Prints the full pathname of the current directory within the sretrive shell.
  get   <file or directory path>     	  : download directory/files into the directory where sretrieve shell cmd was invoked   
  less  <file path>		     	  : Read input file to allows backward movement in the file as well as forward movement 
  cat   <file path>                  	  : show the contents of a file. The output of the cmd can be piped into other system cmd.

  sgrep <search path> <pattern> [options] : Similar to unix grep cmd But with diffrent parameter ordering. The output of the cmd can be piped into other system cmd. 
Part of the Megatest tool suite.
Learn more at http://www.kiatoa.com/fossils/megatest

Version: " megatest-fossil-hash)
)	
;(define (toplevel-command . args) #f)
(define (sretrieve:shell area)
 ; (print area)
  (use readline)
  (let* ((path      '())
	 (prompt    "sretrieve> ")
	 (args      (argv))
         (usr (current-user-name) )   
         (top-areas (sretrieve:get-accessable-projects area))
         (close-port     #f)
         (area-obj  (get-obj-by-code area))
         (user-obj (get-user usr)) 
         (base-path (if (null? area-obj) 
                         "" 
                        (caddr (cdr area-obj))))      
	 (iport     (make-readline-port prompt)))
        (if (null? area-obj)
          (begin 
             (print "Area " area " does not exist")
          (exit 1)))
	(let loop ((inl (read-line iport)))
          ;(print 1) 
	  (if (not (or (or (eof-object? inl)
		       (equal? inl "exit")) (port-closed? iport)))
	      (let* ((parts (string-split inl))
		     (cmd   (if (null? parts) #f (car parts))))
              ;  (print "2")
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
                       ((cat)
		       (let* ((thepath (if (> (length parts) 1) ;; have a parameter
					   (cdr parts)
					   `()))
			      (plen    (length thepath)))
			 (cond
			  ((null? thepath)
                          (print "Error: Missing argument to cat"))
			  ((< plen 2)
                            (sretrieve:shell-cat-cmd path  (car thepath) top-areas base-path '())
                              (sauthorize:do-as-calling-user
                              (lambda ()
			    (run-cmd (conc *sauth-path* "/sauthorize") (list "register-log" (conc "\"" inl "\"") (number->string (car user-obj))  (number->string (caddr area-obj))  "cat")))))

			  (else
                             (sretrieve:shell-cat-cmd path  (car thepath) top-areas base-path (cdr thepath))
                             (sauthorize:do-as-calling-user
                              (lambda ()
			    (run-cmd (conc *sauth-path* "/sauthorize") (list "register-log" (conc "\"" inl "\"") (number->string (car user-obj))  (number->string (caddr area-obj))  "cat"))))
))))
                       ((sgrep)
		       (let* ((thepath (if (> (length parts) 1) ;; have a parameter
					   (cdr parts)
					   `()))
			      (plen    (length thepath)))
			 (cond
			  ((null? thepath) 
                            (print "Error: Missing arguments to grep!! Useage: grep <search path> <pattern> [options] "))
                          ((< plen  2)
                          (print "Error: Missing arguments to grep!! Useage: grep <search path> <pattern> [options] "))
			  (else
                             (sretrieve:shell-grep-cmd path  (car thepath) top-areas base-path  (cdr thepath))
				(sauthorize:do-as-calling-user
                              (lambda ()
			    (run-cmd (conc *sauth-path* "/sauthorize") (list "register-log" (conc "\"" inl "\"") (number->string (car user-obj))  (number->string (caddr area-obj))  "grep"))))))))

                      ((less)
		       (let* ((thepath (if (> (length parts) 1) ;; have a parameter
					   (cdr parts)
					   `()))
			      (plen    (length thepath)))
			 (cond
			  ((null? thepath)
                          (print "Error: Missing argument to less"))
			  ((< plen 2)
                            (sretrieve:shell-less-cmd path  (car thepath) top-areas base-path)
                             (sauthorize:do-as-calling-user
                              (lambda ()
			    (run-cmd (conc *sauth-path* "/sauthorize") (list "register-log" (conc "\"" inl "\"") (number->string (car user-obj))  (number->string (caddr area-obj))  "less")))))
			  (else
                             (print "less cmd takes only one (<file path>) argument!!")))))
                      ((get)
                         (let* ((thepath (if (> (length parts) 1) ;; have a parameter
					   (cdr parts)
					   `()))
			      (plen    (length thepath)))
			 (cond
			  ((null? thepath)
                          (print "Error: Missing argument <path> to get"))
			  ((< plen 2)
                           (let* ((target-path (sauth-common:get-target-path path  (car thepath) top-areas base-path))
				 (restrictions (if (equal? target-path #f)
                                                 ""
                                               (sretrieve:shell-lookup base-path))))
                               (if (not (equal? target-path #f))
                             (begin  
                                 (sretrieve:get-shell-cmd target-path base-path restrictions iport)
                                  (sauthorize:do-as-calling-user
                              (lambda ()
			    (run-cmd (conc *sauth-path* "/sauthorize") (list "register-log" (conc "\"" inl "\"") (number->string (car user-obj))  (number->string (caddr area-obj))  "get"))))))))
			  (else
                            (print "Error: get cmd takes only one argument ")))))
                      ((exit)
                          (print "got exit"))  
                      ((help)
                          (print (sretrieve:shell-help)))
		      (else 
		       (print "Got command: " inl))))
                 (loop (read-line iport)))))))
;;))
    

;;======================================================================
;; MAIN
;;======================================================================
;;(define *default-log-port* (current-error-port))

;(define (sretrieve:load-config exe-dir exe-name)
;  (let* ((fname   (conc exe-dir "/." exe-name ".config")))
;    ;; (ini:property-separator-patt " *  *")
;    ;; (ini:property-separator #\space)
;    (if (file-exists? fname)
;	;; (ini:read-ini fname)
;	(read-config fname #f #f)
;	(make-hash-table))))

;; package-type is "megatest", "builds", "kits" etc.
;;

;(define (sretrieve:load-packages configdat exe-dir package-type)
;  (push-directory exe-dir)
;  (let* ((packages-metadir  (configf:lookup configdat "settings" "packages-metadir"))
;	 (conversion-script (configf:lookup configdat "settings" "conversion-script"))
;	 (upstream-file     (configf:lookup configdat "settings" "upstream-file"))
;	 (package-config    (conc packages-metadir "/" package-type ".config")))
;       (if (file-exists? upstream-file)
;	(if (or (not (file-exists? package-config)) ;; if not created call the updater, otherwise call only if upstream newer
;		(> (file-modification-time upstream-file)(file-modification-time package-config)))
;	    (handle-exceptions
;	     exn
;	     (debug:print 0 "ERROR: failed to run script " conversion-script " with params " upstream-file " " package-config)
;	     (let ((pid (process-run conversion-script (list upstream-file package-config))))
;	       (process-wait pid)))
;	    (debug:print 0 "Skipping update of " package-config " from " upstream-file))
;	(debug:print 0 "Skipping update of " package-config " as " upstream-file " not found"))
;       (let ((res (if (file-exists? package-config)
;		   (begin
;		     (debug:print 0 "Reading package config " package-config)
;		     (read-config package-config #f #t))
;		   (make-hash-table))))
;      (pop-directory)
;      res)))

(define (toplevel-command . args) #f)
(define (sretrieve:process-action  action . args)
    ; (print action)
 ;  (use readline)
    (case (string->symbol action)
      ((get)
       (if (< (length args) 2)
	   (begin 
	     (print  "ERROR: Missing arguments; <area> <relative path>" )
	     (exit 1)))
       (let* ((remargs     (args:get-args args '("-m" ) '() args:arg-hash 0))
              (iport (make-readline-port ">"))
              (area     (car args))
              (usr (current-user-name))
              (area-obj  (get-obj-by-code area))
              (user-obj (get-user usr))
              (top-areas (sretrieve:get-accessable-projects area)) 
              (base-path (if (null? area-obj) 
                                      "" 
                                     (caddr (cdr area-obj))))
	      (sub-path       (if (null? remargs) 
                                       "" 
                                       (car remargs))))

          (if (null? area-obj)
          	    (begin 
             		(print "Area " area " does not exist")
          	         (exit 1)))
              (let* ((target-path (sauth-common:get-target-path '()  (conc area "/" sub-path) top-areas base-path))
		     (restrictions (if (equal? target-path #f)
                                        ""
                                       (sretrieve:shell-lookup base-path))))
             (if (not (equal? target-path #f))
                 (begin  
                   (sauthorize:do-as-calling-user
                      (lambda ()
   		        (run-cmd (conc *sauth-path* "/sauthorize") (list "register-log" (conc "get " area " " sub-path) (number->string (car user-obj))  (number->string (caddr area-obj))  "get"))))
                        (sretrieve:get-shell-cmd-line target-path base-path restrictions  iport))))))
         ((cp)
             (if (< (length args) 2)
	   (begin 
	     (print  "ERROR: Missing arguments; <area> <relative path>" )
	     (exit 1)))
       (let* ((remargs     (args:get-args args '("-m" ) '() args:arg-hash 0))
              (iport (make-readline-port ">"))
              (area     (car args))
              (usr (current-user-name))
              (area-obj  (get-obj-by-code area))
              (user-obj (get-user usr))
              (top-areas (sretrieve:get-accessable-projects area)) 
              (base-path (if (null? area-obj) 
                                      "" 
                                     (caddr (cdr area-obj))))
	      (sub-path       (if (null? remargs) 
                                       "" 
                                       (car remargs))))
         ;  (print args)
          (if (null? area-obj)
          	    (begin 
             		(print "Area " area " does not exist")
          	         (exit 1)))
              (let* ((target-path (sauth-common:get-target-path '()  (conc area "/" sub-path) top-areas base-path))
				 (restrictions (if (equal? target-path #f)
                                                 ""
                                              (sretrieve:shell-lookup base-path))))
                          ;(print target-path) 
                          (if (not (equal? target-path #f))
                             (begin  
                              (sauthorize:do-as-calling-user
                              (lambda ()
			    (run-cmd (conc *sauth-path* "/sauthorize") (list "register-log" (conc "get " area " " sub-path) (number->string (car user-obj))  (number->string (caddr area-obj))  "get"))))
                            (sretrieve:get-shell-cmd-line target-path base-path restrictions  iport))))))
      ((ls)
          (cond
            ((< (length args) 1)
              (begin 
	      (print  "ERROR: Missing arguments; <area> ")
	      (exit 1)))
              ((equal? (length args) 1)
                 (let*  ((area     (car args))
                         (usr (current-user-name))
                         (area-obj  (get-obj-by-code area))
                         (user-obj (get-user usr))
                         (top-areas (sretrieve:get-accessable-projects area)) 
                         (base-path (if (null? area-obj) 
                                      "" 
                                     (caddr (cdr area-obj)))))
                  (if (null? area-obj)
          	    (begin 
             		(print "Area " area " does not exist")
          	         (exit 1)))
           	 (sauth-common:shell-ls-cmd '() area top-areas base-path  '())
                 (sauthorize:do-as-calling-user
                   (lambda ()
		    (run-cmd (conc *sauth-path* "/sauthorize") (list "register-log" "ls" (number->string (car user-obj)) (number->string (caddr area-obj)) "ls"))))))
             ((> (length args) 1)
               (let*  ((remargs     (args:get-args args '("-m" ) '() args:arg-hash 0))
                        (usr (current-user-name))
                        (user-obj (get-user usr))
                         (area     (car args)))
                         (let* ((area-obj  (get-obj-by-code area))
                               (top-areas (sretrieve:get-accessable-projects area)) 
                               (base-path (if (null? area-obj) 
                                      "" 
                                     (caddr (cdr area-obj))))
                                 
                               (sub-path (if (null? remargs) 
                                       area 
                                      (conc area "/" (car remargs)))))
                             ;(print "sub path "  sub-path)
                            (if (null? area-obj)
          	              (begin 
             		        (print "Area " area " does not exist")
          	                 (exit 1)))
                              (sauth-common:shell-ls-cmd `()  sub-path top-areas base-path '())
                            (sauthorize:do-as-calling-user
				(lambda ()
                       	       (run-cmd (conc *sauth-path* "/sauthorize") (list "register-log" (conc "ls " sub-path) (number->string (car user-obj)) (number->string (caddr area-obj)) "ls")))))))))

       ((shell)
          (if (< (length args) 1)
             (begin 
	     (print  "ERROR: Missing arguments <area>!!" )
	     (exit 1))
             (sretrieve:shell (car args)))) 
      (else (print 0 "Unrecognised command " action))))

(define (main)
  (let* ((args      (argv))
	 (prog      (car args))
	 (rema      (cdr args))
	 (exe-name  (pathname-file (car (argv))))
	 (exe-dir   (or (pathname-directory prog)
			(sretrieve:find exe-name (string-split (get-environment-variable "PATH") ":"))))
	 ;(configdat (sretrieve:load-config exe-dir exe-name))
)
    ;; preserve the exe data in the config file
    ;(hash-table-set! configdat "exe-info" (list (list "exe-name" exe-name)
					;	(list "exe-dir"  exe-dir)))
    (cond
     ;; one-word commands
     ((eq? (length rema) 1)
      (case (string->symbol (car rema))
	((help -h -help --h --help)
	 (print sretrieve:help))
	(else
	 (print "ERROR: Unrecognised command. Try \"sretrieve help\""))))
     ;; multi-word commands
     ((null? rema)(print sretrieve:help))
     ((>= (length rema) 2)
      
      (apply sretrieve:process-action  (car rema) (cdr rema)))
     (else (debug:print 0 "ERROR: Unrecognised command. Try \"sretrieve help\"")))))

(main)


      
