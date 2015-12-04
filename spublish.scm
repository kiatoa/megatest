
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

(require-library iup)
(import (prefix iup iup:))
(require-library ini-file)
(import (prefix ini-file ini:))

(use canvas-draw)
(import canvas-draw-iup)

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

Note: run spublish without parameters to start the gui.

  ls                 : list contents of target area
  cp <file>          : copy file to target area
  rm <file>          : remove file <file> from target area
  log                :

  options:

    -m \"message\"   : describe what was done

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
(define (spublish:cp configdat submitter source-path target-dir targ-file comment)
  (let ((targ-path (conc target-dir "/" targ-file)))
    (if (file-exists? targ-path)
	(begin
	  (print "ERROR: target file already exists, remove it before re-publishing")
	  (exit 1)))
    (spublish:db-do
     configdat
     (lambda (db)
       (spublish:register-action db "cp" submitter source-path comment)))
    (let* (;; (target-path (configf:lookup "settings" "target-path"))
	   (th1         (make-thread
			 (lambda ()
			   (file-copy source-path targ-path #t))
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
       (spublish:register-action db "rm" submitter "" comment)))
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

;;======================================================================
;; GUI
;;======================================================================

;; The main menu 
(define (spublish:main-menu)
  (iup:menu ;; a menu is a special attribute to a dialog (think Gnome putting the menu at screen top)
   (iup:menu-item "Files" (iup:menu   ;; Note that you can use either #:action or action: for options
		       (iup:menu-item "Open"  action: (lambda (obj)
							(iup:show (iup:file-dialog))
							(print "File->open " obj)))
		       (iup:menu-item "Save"  #:action (lambda (obj)(print "File->save " obj)))
		       (iup:menu-item "Exit"  #:action (lambda (obj)(exit)))))
   (iup:menu-item "Tools" (iup:menu
		       (iup:menu-item "Create new blah" #:action (lambda (obj)(print "Tools->new blah")))
		       ;; (iup:menu-item "Show dialog"     #:action (lambda (obj)
		       ;;  					   (show message-window
		       ;;  					     #:modal? #t
		       ;;  					     ;; set positon using coordinates or center, start, top, left, end, bottom, right, parent-center, current
		       ;;  					     ;; #:x 'mouse
		       ;;  					     ;; #:y 'mouse
		       ;;  )					     
		       ))))

(define (spublish:publish-view configdat)
  ;; (pp (hash-table->alist configdat))
  (let* ((areas       (configf:get-section configdat "areas"))
	 (label-size  "70x")
	 (areas-sel   (iup:listbox #:expand "HORIZONTAL" #:dropdown "YES"))
	 (version-tb  (iup:textbox #:expand "HORIZONTAL")) ;;  #:size "50x"))
	 (areas-sel   (iup:listbox #:expand "HORIZONTAL" #:dropdown "YES"))
	 (component   (iup:listbox #:expand "HORIZONTAL" #:dropdown "YES" ))
	 (version-val (iup:textbox #:expand "HORIZONTAL" #:size "50x"))
	 ;; (copy-link   (iup:toggle  #:expand "HORIZONTAL"))
	 ;; (iteration   (iup:textbox #:expand "YES" #:size "20x"))
	 ;; (iteration   (iup:textbox #:expand "HORIZONTAL" #:size "20x"))
	 (area-filter (iup:textbox #:expand "HORIZONTAL" #:value "%"))
	 (comment-tb  (iup:textbox #:expand "YES" #:multiline "YES"))
	 (source-tb   (iup:textbox #:expand "HORIZONTAL"
				   #:value (or (configf:lookup configdat "settings" "basepath")
					       "")))
	 (publish     (lambda (publish-type)
			(let* ((area-num    (or (string->number (iup:attribute areas-sel "VALUE")) 0))
			       (area-dat    (if (> area-num 0)(list-ref areas (- area-num 1))'("NOT SELECTED" "NOT SELECTED")))
			       (area-path   (cadr area-dat))
			       (area-name   (car  area-dat))
			       (version     (iup:attribute version-tb "VALUE"))
			       (comment     (iup:attribute comment-tb "VALUE"))
			       (spath       (iup:attribute source-tb  "VALUE"))
			       (submitter   (current-user-name))
			       (quality     2))
			  (spublish:publish configdat publish-type area-name version comment spath submitter quality))))
	 (copy        (iup:button "Copy and Publish"
				  #:expand "HORIZONTAL"
				  #:action (lambda (obj)
					     (publish 'copy))))
	 (link        (iup:button "Link and Publish"
				  #:expand "HORIZONTAL"
				  #:action (lambda (obj)
					     (publish 'link))))
	 (browse-btn  (iup:button "Browse"
				  #:size "40x"
				  #:action (lambda (obj)
					     (let* ((fd  (iup:file-dialog #:dialogtype "DIR"))
						    (top (iup:show fd #:modal? "YES")))
					       (iup:attribute-set! source-tb "VALUE"
								   (iup:attribute fd "VALUE"))
					       (iup:destroy! fd))))))
    (print "areas")
    ;; (pp areas)
    (fold (lambda (areadat num)
	    ;; (print "Adding num=" num ", areadat=" areadat)
	    (iup:attribute-set! areas-sel (conc num) (car areadat))
	    (+ 1 num))
	  1 areas)
    (iup:vbox
     (iup:hbox (iup:label "Area:"        #:size label-size) ;; area-filter 
	       areas-sel)
     (iup:hbox (iup:label "Version:"     #:size label-size)   version-tb)
     ;; (iup:hbox (iup:label "Link only"    #:size label-size)   copy-link)
     ;; 	       (iup:label "Iteration:")   iteration)
     (iup:hbox (iup:label "Comment:"     #:size label-size)   comment-tb)
     (iup:hbox (iup:label "Source base path:" #:size label-size)   source-tb browse-btn)
     (iup:hbox copy link))))

(define (spublish:lst->path pathlst)
  (conc "/" (string-intersperse (map conc pathlst) "/")))

(define (spublish:path->lst path)
  (string-split path "/"))

(define (spublish:pathdat-apply-heuristics configdat path)
  (cond
   ((file-exists? path) "found")
   (else (conc path " not installed"))))

(define (spublish:get-view configdat)
  (iup:vbox
   (iup:hbox
    (let* ((label-size     "60x")
	   ;; filter elements
	   (area-filter    "%")
	   (version-filter "%")
	   (iter-filter    ">= 0")
	   ;; reverse lookup from path to data for src and installed
	   (srcdat         (make-hash-table)) ;; reverse lookup
	   (installed-dat  (make-hash-table))
	   ;; config values
	   (basepath       (configf:lookup configdat "settings" "basepath"))
	   ;; gui elements
	   (submitter      (iup:label "" #:expand "HORIZONTAL"))
	   (date-submitted (iup:label "" #:expand "HORIZONTAL"))
	   (comment        (iup:label "" #:expand "HORIZONTAL"))
	   (copy-link      (iup:label "" #:expand "HORIZONTAL"))
	   (quality        (iup:label "" #:expand "HORIZONTAL"))
	   (installed-status (iup:label "" #:expand "HORIZONTAL"))
	   ;; misc 
	   (curr-record    #f)
	   ;; (source-data    (iup:label "" #:expand "HORIZONTAL"))
	   (tb             (iup:treebox
			    #:value 0
			    #:name "Packages"
			    #:expand "YES"
			    #:addexpanded "NO"
			    #:selection-cb
			    (lambda (obj id state)
			      ;; (print "obj: " obj ", id: " id ", state: " state)
			      (let* ((path   (spublish:lst->path (cdr (tree:node->path obj id))))
				     (record (hash-table-ref/default srcdat path #f)))
				(if record
				    (begin
				      (set! curr-record record)
				      (iup:attribute-set! submitter      "TITLE" (spublish:pkg-get-submitter record))
				      (iup:attribute-set! date-submitted "TITLE" (time->string (seconds->local-time (spublish:pkg-get-datetime record))))
				      (iup:attribute-set! comment        "TITLE" (spublish:pkg-get-comment record))
				      (iup:attribute-set! quality        "TITLE" (spublish:pkg-get-quality record))
				      (iup:attribute-set! copy-link      "TITLE" (spublish:pkg-get-store_type record))
				      ))
				;; (print  "id=" id " path=" path " record=" record);; (tree:node->path obj id) " run-id: " run-id)
				))))
	   (tb2             (iup:treebox
			    #:value 0
			    #:name "Installed"
			    #:expand "YES"
			    #:addexpanded "NO"
			    #:selection-cb
			    (lambda (obj id state)
			      ;; (print "obj: " obj ", id: " id ", state: " state)
			      (let* ((path   (spublish:lst->path (cdr (tree:node->path obj id))))
				     (status (hash-table-ref/default installed-dat path #f)))
				(iup:attribute-set! installed-status "TITLE" (if status status ""))
				))))
	   (refresh        (lambda (obj)
			     (let* ((db    (spublish:open-db configdat))
				    (areas (or (configf:get-section configdat "areas") '())))
			       ;;
			       ;; first update the Sources
			       ;;
			       (for-each
				(lambda (pkgitem)
				  (let* ((pkg-path   (list (spublish:pkg-get-area  pkgitem)
							   (spublish:pkg-get-version_name pkgitem)
							   (spublish:pkg-get-iteration pkgitem)))
					 (pkg-id     (spublish:pkg-get-id          pkgitem))
					 (path       (spublish:lst->path pkg-path)))
				    ;; (print "tree:add-node tb=" tb ", pkg-path=" pkg-path ", pkg-id=" pkg-id)
				    (if (not (hash-table-ref/default srcdat path #f))
					(tree:add-node tb "Packages" pkg-path userdata: (conc "pkg-id: " pkg-id)))
				    ;; (print "path=" path " pkgitem=" pkgitem)
				    (hash-table-set! srcdat path pkgitem)))
				(spublish:get-pkgs db area-filter version-filter iter-filter))
			       ;;
			       ;; then update the installed
			       ;;
			       (for-each
				(lambda (area)
				  (let* ((path     (conc "/" (cadr area)))
					 (fullpath (conc basepath path)))
				    (if (not (hash-table-ref/default installed-dat path #f))
					(tree:add-node tb2 "Installed" (spublish:path->lst path)))
				    (hash-table-set! installed-dat path (spublish:pathdat-apply-heuristics configdat fullpath))))
				areas)
			       (sqlite3:finalize! db))))
	   (apply          (iup:button "Apply"
				       #:action
				       (lambda (obj)
					 (if curr-record
					     (let* ((area        (spublish:pkg-get-area        curr-record))
						    (stored-path (spublish:pkg-get-stored_path curr-record))
						    (source-type (spublish:pkg-get-store_type  curr-record))
						    (source-path (case source-type ;;  (equal? source-type "link"))
								   ((link)(spublish:pkg-get-source-path curr-record))
								   ((copy)stored-path)
								   (else #f)))
						    (dest-stub   (configf:lookup configdat "areas" area))
						    (target-path (conc basepath "/" dest-stub)))
					       (spublish:build-dir-make-link stored-path target-path)
					       (print "Creating link from " stored-path " to " target-path)))))))
      (iup:vbox 
       (iup:hbox tb tb2)
       (iup:frame 
	#:title "Source Info"
	(iup:vbox
	 (iup:hbox (iup:button "Refresh" #:action refresh) apply)
	 (iup:hbox (iup:label "Submitter: ") ;;  #:size label-size)
		   submitter 
		   (iup:label "Submitted on: ") ;;  #:size label-size)
		   date-submitted)
	 (iup:hbox (iup:label "Data stored: ")
		   copy-link
		   (iup:label "Quality: ")
		   quality)
	 (iup:hbox (iup:label "Comment: ")
		   comment)))
       (iup:frame
	#:title "Installed Info"
	(iup:vbox
	 (iup:hbox (iup:label "Installed status/path: ") installed-status)))
       )))))

(define (spublish:manage-view configdat)
  (iup:vbox
   (iup:hbox 
    (iup:button "Pushme"
		#:expand "YES"
		))))

(define (spublish:gui configdat)
  (iup:show
   (iup:dialog 
    #:title (conc "Spublish dashboard " (current-user-name) ":" (current-directory))   
    #:menu (spublish:main-menu)
    (let* ((tabs (iup:tabs
		  #:tabchangepos-cb (lambda (obj curr prev)
				      (set! *spublish:current-tab-number* curr))
		  (spublish:publish-view configdat)
		  (spublish:get-view configdat)
		  (spublish:manage-view configdat)
		  )))
	;; (set! (iup:callback tabs tabchange-cb:) (lambda (a b c)(print "SWITCHED TO TAB: " a " " b " " c)))
	(iup:attribute-set! tabs "TABTITLE0" "Publish")
	(iup:attribute-set! tabs "TABTITLE1" "Get")
	(iup:attribute-set! tabs "TABTITLE2" "Manage")
	;; (iup:attribute-set! tabs "BGCOLOR" "190 190 190")
	tabs)))
  (iup:main-loop))

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
       (if (< (length args) 1)
	   (begin 
	     (print "ERROR: Missing arguments; " (string-intersperse args ", "))
	     (exit 1)))
       (let* ((remargs     (args:get-args args '("-m") '() args:arg-hash 0))
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
	 (spublish:cp configdat user src-path target-dir targ-file msg)))
      ((rm)
       (if (< (length args) 1)
	   (begin 
	     (print "ERROR: Missing arguments; " (string-intersperse args ", "))
	     (exit 1)))
       (let* (;; (remargs     (args:get-args args '("-m") '() args:arg-hash 0))
	      (targ-file (car args))
	      ;; (src-path    (with-input-from-pipe
	      ;;   	    (conc "readlink -f " src-path-in)
	      ;;   	    (lambda ()
	      ;;   	      (read-line))))
	      (msg         (or (args:get-arg "-m") "")))
;;	      (targ-file   (pathname-strip-directory src-path)))
	 (print "attempting to remove " targ-file " from " target-dir)
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
(let ((debugcontrolf (conc (get-environment-variable "HOME") "/.spublishrc")))
  (if (file-exists? debugcontrolf)
      (load debugcontrolf)))

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
	(else
	 (print "ERROR: Unrecognised command. Try \"spublish help\""))))
     ;; multi-word commands
     ((null? rema)(spublish:gui configdat))
     ((>= (length rema) 2)
      (apply spublish:process-action configdat (car rema)(cdr rema)))
     (else (print "ERROR: Unrecognised command. Try \"spublish help\"")))))

(main)
