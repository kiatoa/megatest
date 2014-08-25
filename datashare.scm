
;; Copyright 2006-2013, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

(use ssax)
(use sxml-serializer)
(use sxml-modifications)
(use regex)
(use srfi-69)
(use regex-case)
(use posix)
(use json)
(use csv)
(use srfi-18)
(use format)

(require-library iup)
(import (prefix iup iup:))
(require-library ini-file)
(import (prefix ini-file ini:))

(use canvas-draw)
(import canvas-draw-iup)

(use sqlite3 srfi-1 posix regex regex-case srfi-69)
(import (prefix sqlite3 sqlite3:))

(declare (uses configf))
(declare (uses tree))
;; (declare (uses dcommon))
;; (declare (uses margs))
;; (declare (uses launch))
;; (declare (uses gutils))
;; (declare (uses db))
;; (declare (uses synchash))
;; (declare (uses server))
;; (declare (uses megatest-version))
;; (declare (uses tbd))

(include "megatest-fossil-hash.scm")

;;
;; GLOBALS
;;
(define *datashare:current-tab-number* 0)
(define datashare:help (conc "Usage: datashare [action [params ...]]

Note: run datashare without parameters to start the gui.

  publish <area> <key> [group]        : Publish data to share, use group to protect (i)
  get <area> <key> [destpath]         : Get a link to data, put the link in destpath (ii)
  update <area> <key>                 : Update the link to data to the latest iteration.

(i)  Uses group ownership of files to be published for group if not specified
(ii) Uses local path or looks up script to find path in configs

Part of the Megatest tool suite. Learn more at http://www.kiatoa.com/fossils/megatest

Version: " megatest-fossil-hash)) ;; "

;;======================================================================
;; RECORDS
;;======================================================================

;; make-vector-record "testing" datastore pkg id area version_name store_type copied source_path iteration submitter datetime storegrp datavol quality disk_id comment
;; testing
(define (make-datastore:pkg)(make-vector 15))
(define-inline (datastore:pkg-get-id             vec)    (vector-ref  vec 0))
(define-inline (datastore:pkg-get-area           vec)    (vector-ref  vec 1))
(define-inline (datastore:pkg-get-version_name   vec)    (vector-ref  vec 2))
(define-inline (datastore:pkg-get-store_type     vec)    (vector-ref  vec 3))
(define-inline (datastore:pkg-get-copied         vec)    (vector-ref  vec 4))
(define-inline (datastore:pkg-get-source_path    vec)    (vector-ref  vec 5))
(define-inline (datastore:pkg-get-iteration      vec)    (vector-ref  vec 6))
(define-inline (datastore:pkg-get-submitter      vec)    (vector-ref  vec 7))
(define-inline (datastore:pkg-get-datetime       vec)    (vector-ref  vec 8))
(define-inline (datastore:pkg-get-storegrp       vec)    (vector-ref  vec 9))
(define-inline (datastore:pkg-get-datavol        vec)    (vector-ref  vec 10))
(define-inline (datastore:pkg-get-quality        vec)    (vector-ref  vec 11))
(define-inline (datastore:pkg-get-disk_id        vec)    (vector-ref  vec 12))
(define-inline (datastore:pkg-get-comment        vec)    (vector-ref  vec 13))
(define-inline (datastore:pkg-get-stored_path    vec)    (vector-ref  vec 14))
(define-inline (datastore:pkg-set-id!            vec val)(vector-set! vec 0 val))
(define-inline (datastore:pkg-set-area!          vec val)(vector-set! vec 1 val))
(define-inline (datastore:pkg-set-version_name!  vec val)(vector-set! vec 2 val))
(define-inline (datastore:pkg-set-store_type!    vec val)(vector-set! vec 3 val))
(define-inline (datastore:pkg-set-copied!        vec val)(vector-set! vec 4 val))
(define-inline (datastore:pkg-set-source_path!   vec val)(vector-set! vec 5 val))
(define-inline (datastore:pkg-set-iteration!     vec val)(vector-set! vec 6 val))
(define-inline (datastore:pkg-set-submitter!     vec val)(vector-set! vec 7 val))
(define-inline (datastore:pkg-set-datetime!      vec val)(vector-set! vec 8 val))
(define-inline (datastore:pkg-set-storegrp!      vec val)(vector-set! vec 9 val))
(define-inline (datastore:pkg-set-datavol!       vec val)(vector-set! vec 10 val))
(define-inline (datastore:pkg-set-quality!       vec val)(vector-set! vec 11 val))
(define-inline (datastore:pkg-set-disk_id!       vec val)(vector-set! vec 12 val))
(define-inline (datastore:pkg-set-comment!       vec val)(vector-set! vec 13 val))
(define-inline (datastore:pkg-set-stored_path!   vec val)(vector-set! vec 14 val))

;;======================================================================
;; DB
;;======================================================================

(define (datashare:initialize-db db)
  (for-each
   (lambda (qry)
     (sqlite3:execute db qry))
   (list 
    "CREATE TABLE pkgs 
         (id           INTEGER PRIMARY KEY,
          area         TEXT,
          version_name TEXT,
          store_type   TEXT DEFAULT 'copy',
          copied       INTEGER DEFAULT 0,
          source_path  TEXT,
          stored_path  TEXT,
          iteration    INTEGER DEFAULT 0,
          submitter    TEXT,
          datetime     TIMESTAMP DEFAULT (strftime('%s','now')),
          storegrp     TEXT,
          datavol      INTEGER,
          quality      TEXT,
          disk_id      INTEGER,
          comment      TEXT);"
    "CREATE TABLE refs
         (id        INTEGER PRIMARY KEY,
          pkg_id    INTEGER,
          destlink  TEXT);"
    "CREATE TABLE disks
         (id         INTEGER PRIMARY KEY,
          storegrp   TEXT,
          path       TEXT);")))

(define (datashare:register-data db area version-name store-type submitter quality source-path comment)
  (let ((iter-qry       (sqlite3:prepare db "SELECT max(iteration) FROM pkgs WHERE area=? AND version_name=?;"))
	(next-iteration 0))
    (sqlite3:with-transaction
     db
     (lambda ()
       (sqlite3:for-each-row
	(lambda (iteration)
	  (if (and (number? iteration)
		   (>= iteration next-iteration))
	      (set! next-iteration (+ iteration 1))))
	iter-qry area version-name)
       ;; now store the data
       (sqlite3:execute db "INSERT INTO pkgs (area,version_name,iteration,store_type,submitter,source_path,quality,comment) 
                                 VALUES (?,?,?,?,?,?,?,?);"
			area version-name next-iteration (conc store-type) submitter source-path quality comment)))
    (sqlite3:finalize! iter-qry)
    next-iteration))

(define (datastore:get-id db area version-name iteration)
  (let ((res #f))
    (sqlite3:for-each-row
     (lambda (id)
       (set! res id))
     db
     "SELECT id FROM pkgs WHERE area=? AND version_name=? AND iteration=?;"
     area version-name iteration)
    res))

(define (datastore:set-stored-path db id path)
  (sqlite3:execute db "UPDATE pkgs SET stored_path=? WHERE id=?;" path id))

(define (datastore:set-copied db id value)
  (sqlite3:execute db "UPDATE pkgs SET copied=? WHERE id=?;" value id))
  
(define (datashare:get-pkg-record db area version-name iteration)
  #f)

;; Create the sqlite db
(define (datashare:open-db configdat) 
  (let ((path (configf:lookup configdat "database" "location")))
    (if (and path
	     (directory? path)
	     (file-read-access? path))
	(let* ((dbpath    (conc path "/datashare.db"))
	       (writeable (file-write-access? dbpath))
	       (dbexists  (file-exists? dbpath))
	       (handler   (make-busy-timeout 136000)))
	  (handle-exceptions
	   exn
	   (begin
	     (debug:print 2 "ERROR: problem accessing db " dbpath
			  ((condition-property-accessor 'exn 'message) exn))
	     (exit))
	   (set! db (sqlite3:open-database dbpath)))
	  (if *db-write-access* (sqlite3:set-busy-handler! db handler))
	  (if (not dbexists)
	      (begin
		(datashare:initialize-db db)))
	  db)
	(print "ERROR: invalid path for storing database: " path))))

(define (open-run-close-exception-handling proc idb . params)
  (handle-exceptions
   exn
   (let ((sleep-time (random 30))
         (err-status ((condition-property-accessor 'sqlite3 'status #f) exn)))
     (case err-status
       ((busy)
        (thread-sleep! sleep-time))
       (else
        (print "EXCEPTION: database overloaded or unreadable.")
        (print " message: " ((condition-property-accessor 'exn 'message) exn))
        (print "exn=" (condition->list exn))
        (print " status:  " ((condition-property-accessor 'sqlite3 'status) exn))
        (print-call-chain)
        (thread-sleep! sleep-time)
        (print "trying db call one more time....this may never recover, if necessary kill process " (current-process-id) " on host " (get-host-name) " to clean up")))
     (apply open-run-close-exception-handling proc idb params))
   (apply open-run-close-no-exception-handling proc idb params)))

(define (open-run-close-no-exception-handling  proc idb . params)
  ;; (print "open-run-close-no-exception-handling START given a db=" (if idb "yes " "no ") ", params=" params)
  (let* ((db (cond
	      ((sqlite3:database? idb)     idb)
	      ((not idb)                   (print "ERROR: cannot open-run-close with #f anymore"))
	      ((procedure? idb)            (idb))
	      (else                        (print "ERROR: cannot open-run-close with #f anymore"))))
	 (res #f))
    (set! res (apply proc db params))
    (if (not idb)(sqlite3:finalize! dbstruct))
    ;; (print "open-run-close-no-exception-handling END" )
    res))

(define open-run-close open-run-close-no-exception-handling)

(define (datashare:get-pkgs db area-filter version-filter iter-filter)
  (let ((res '()))
    (sqlite3:for-each-row ;; replace with fold ...
     (lambda (a . b)
       (set! res (cons (list->vector (cons a b)) res)))
     db 
     (conc "SELECT id,area,version_name,store_type,copied,source_path,iteration,submitter,datetime,storegrp,datavol,quality,disk_id,comment,stored_path "
	   " FROM pkgs WHERE area like ? AND version_name LIKE ? AND iteration " iter-filter ";")
     area-filter version-filter)
    (reverse res)))

;;======================================================================
;; DATA IMPORT/EXPORT
;;======================================================================

(define (datashare:import-data configdat source-path dest-path area version iteration)
  (let* ((space-avail (car dest-path))
	 (disk-path   (cdr dest-path))
	 (targ-path   (conc disk-path "/" area "/" version "/" iteration))
	 (id          (datastore:get-id db area version iteration))
	 (db          (datashare:open-db configdat)))
    (if (> space-avail 10000) ;; dumb heuristic
	(begin
	  (create-directory targ-path #t)
	  (datastore:set-stored-path db id targ-path)
	  (print "Running command: rsync -av " source-path " " targ-path)
	  (let ((th1 (make-thread (lambda ()
				    (let ((pid (process-run "rsync" (list "-av" source-path targ-path))))
				      (process-wait pid)
				      (datastore:set-copied db id "yes")
				      (sqlite3:finalize! db)))
				   "Data copy")))
	    (thread-start! th1))
	  #t)
	(begin
	  (print "ERROR: Not enough space in storage area " dest-path)
	  (datastore:set-copied db id "no")
	  (sqlite3:finalize! db)
	  #f))))

(define (datastore:get-best-storage configdat)
  (let* ((storage     (configf:lookup configdat "settings" "storage"))
	 (store-areas (if storage (string-split storage) '())))
    (print "Looking for available space in " store-areas)
    (datastore:find-most-space store-areas)))

;; (string->number (list-ref (with-input-from-pipe "df -B1000000 /tmp" (lambda ()(read-line)(string-split (read-line)))) 3))

(define (datastore:find-most-space paths)
  (fold (lambda (area res)
	  ;; (print "area=" area " res=" res)
	  (let ((maxspace (car res))
		(currpath (cdr res)))
	    ;; (print currpath " " maxspace)
	    (if (file-write-access? area)
		(let ((currspace (string->number
				  (list-ref
				   (with-input-from-pipe 
				    ;; (conc "df --output=avail " area)
				    (conc "df -B1000000 " area)
				    ;; (lambda ()(read)(read))
				    (lambda ()(read-line)(string-split (read-line))))
				   3))))
		  (if (> currspace maxspace) 
		      (cons currspace area)
		      res))
		res)))
	(cons 0 #f)
	paths))

;; remove existing link and if possible ...
;; create path to next of tip of target, create link back to source
(define (datastore:build-dir-make-link source target)
  (if (file-exists? target)(datastore:backup-move target))
  (create-directory (pathname-directory target) #t)
  (create-symbolic-link source target))

(define (datastore:backup-move path)
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
(define (datashare:main-menu)
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

(define (datashare:publish-view configdat)
  (pp (hash-table->alist configdat))
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
			       (quality     2)
			       ;; (import-type (if (equal? (iup:attribute copy-link "VALUE") "ON" )
			       ;;  		'copy
			       ;;  		'link))
			       (db          (datashare:open-db configdat))
			       (iteration   (datashare:register-data db area-name version publish-type submitter quality spath comment))
			       (dest-store  (datastore:get-best-storage configdat)))
			  (if iteration
			      (if (eq? 'copy publish-type)
				  (datashare:import-data configdat spath dest-store area-name version iteration))
			      (print "ERROR: Failed to get an iteration number"))
			  (sqlite3:finalize! db))))
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
    (pp areas)
    (fold (lambda (areadat num)
	    ;; (print "Adding num=" num ", areadat=" areadat)
	    (iup:attribute-set! areas-sel (conc num) (car areadat))
	    (+ 1 num))
	  1 areas)
    (iup:vbox
     (iup:hbox (iup:label "Area:"        #:size label-size)   areas-sel)
     (iup:hbox (iup:label "Version:"     #:size label-size)   version-tb)
     ;; (iup:hbox (iup:label "Link only"    #:size label-size)   copy-link)
     ;; 	       (iup:label "Iteration:")   iteration)
     (iup:hbox (iup:label "Comment:"     #:size label-size)   comment-tb)
     (iup:hbox (iup:label "Source base path:" #:size label-size)   source-tb browse-btn)
     (iup:hbox copy link))))

(define (datastore:lst->path pathlst)
  (conc "/" (string-intersperse (map conc pathlst) "/")))

(define (datastore:path->lst path)
  (string-split path "/"))

(define (datastore:pathdat-apply-heuristics configdat path)
  (cond
   ((file-exists? path) "found")
   (else (conc path " not installed"))))

(define (datashare:get-view configdat)
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
			      (let* ((path   (datastore:lst->path (cdr (tree:node->path obj id))))
				     (record (hash-table-ref/default srcdat path #f)))
				(if record
				    (begin
				      (set! curr-record record)
				      (iup:attribute-set! submitter      "TITLE" (datastore:pkg-get-submitter record))
				      (iup:attribute-set! date-submitted "TITLE" (time->string (seconds->local-time (datastore:pkg-get-datetime record))))
				      (iup:attribute-set! comment        "TITLE" (datastore:pkg-get-comment record))
				      (iup:attribute-set! quality        "TITLE" (datastore:pkg-get-quality record))
				      (iup:attribute-set! copy-link      "TITLE" (datastore:pkg-get-store_type record))
				      ))
				(print  "id=" id " path=" path " record=" record);; (tree:node->path obj id) " run-id: " run-id)
				))))
	   (tb2             (iup:treebox
			    #:value 0
			    #:name "Installed"
			    #:expand "YES"
			    #:addexpanded "NO"
			    #:selection-cb
			    (lambda (obj id state)
			      ;; (print "obj: " obj ", id: " id ", state: " state)
			      (let* ((path   (datastore:lst->path (cdr (tree:node->path obj id))))
				     (status (hash-table-ref/default installed-dat path #f)))
				(iup:attribute-set! installed-status "TITLE" (if status status ""))
				))))
	   (refresh        (lambda (obj)
			     (let* ((db    (datashare:open-db configdat))
				    (areas (or (configf:get-section configdat "areas") '())))
			       ;;
			       ;; first update the Sources
			       ;;
			       (for-each
				(lambda (pkgitem)
				  (let* ((pkg-path   (list (datastore:pkg-get-area  pkgitem)
							   (datastore:pkg-get-version_name pkgitem)
							   (datastore:pkg-get-iteration pkgitem)))
					 (pkg-id     (datastore:pkg-get-id          pkgitem))
					 (path       (datastore:lst->path pkg-path)))
				    ;; (print "tree:add-node tb=" tb ", pkg-path=" pkg-path ", pkg-id=" pkg-id)
				    (if (not (hash-table-ref/default srcdat path #f))
					(tree:add-node tb "Packages" pkg-path userdata: (conc "pkg-id: " pkg-id)))
				    ;; (print "path=" path " pkgitem=" pkgitem)
				    (hash-table-set! srcdat path pkgitem)))
				(datashare:get-pkgs db area-filter version-filter iter-filter))
			       ;;
			       ;; then update the installed
			       ;;
			       (for-each
				(lambda (area)
				  (let* ((path     (conc "/" (cadr area)))
					 (fullpath (conc basepath path)))
				    (if (not (hash-table-ref/default installed-dat path #f))
					(tree:add-node tb2 "Installed" (datastore:path->lst path)))
				    (hash-table-set! installed-dat path (datastore:pathdat-apply-heuristics configdat fullpath))))
				areas)
			       (sqlite3:finalize! db))))
	   (apply          (iup:button "Apply"
				       #:action
				       (lambda (obj)
					 (if curr-record
					     (let* ((area        (datastore:pkg-get-area        curr-record))
						    (stored-path (datastore:pkg-get-stored_path curr-record))
						    (source-type (datastore:pkg-get-store_type  curr-record))
						    (source-path (case source-type ;;  (equal? source-type "link"))
								   ((link)(datastore:pkg-get-source-path curr-record))
								   ((copy)stored-path)
								   (else #f)))
						    (dest-stub   (configf:lookup configdat "areas" area))
						    (target-path (conc basepath "/" dest-stub)))
					       (datastore:build-dir-make-link stored-path target-path)
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

(define (datashare:manage-view configdat)
  (iup:vbox
   (iup:hbox 
    (iup:button "Pushme"
		#:expand "YES"
		))))

(define (datashare:gui configdat)
  (iup:show
   (iup:dialog 
    #:title (conc "DataShare dashboard " (current-user-name) ":" (current-directory))
    #:menu (datashare:main-menu)
    (let* ((tabs (iup:tabs
		  #:tabchangepos-cb (lambda (obj curr prev)
				      (set! *datashare:current-tab-number* curr))
		  (datashare:publish-view configdat)
		  (datashare:get-view configdat)
		  (datashare:manage-view configdat)
		  )))
	;; (set! (iup:callback tabs tabchange-cb:) (lambda (a b c)(print "SWITCHED TO TAB: " a " " b " " c)))
	(iup:attribute-set! tabs "TABTITLE0" "Publish")
	(iup:attribute-set! tabs "TABTITLE1" "Get")
	(iup:attribute-set! tabs "TABTITLE2" "Manage")
	;; (iup:attribute-set! tabs "BGCOLOR" "190 190 190")
	tabs)))
  (iup:main-loop))

;;======================================================================
;; MAIN
;;======================================================================

(define (datashare:load-config path)
  (let* ((exename (pathname-file (car (argv))))
	 (fname   (conc path "/." exename ".config")))
    (ini:property-separator-patt " *  *")
    (ini:property-separator #\space)
    (if (file-exists? fname)
	;; (ini:read-ini fname)
	(read-config fname #f #t)
	(make-hash-table))))

(define (main)
  (let* ((args      (argv))
	 (prog      (car args))
	 (rema      (cdr args))
	 (configdat (datashare:load-config (pathname-directory prog))))
    (cond
     ((eq? (length rema) 1)
      (case (string->symbol (car rema))
	((help -h -help --h --help)
	 (print datashare:help))
	(else
	 (print "ERROR: Unrecognised command. Try \"datashare help\""))))
     ((null? rema)(datashare:gui configdat))
     ((>= (length rema) 2)
      (apply process-action (car rema)(cdr rema)))
     (else (print "ERROR: Unrecognised command. Try \"datashare help\"")))))

(main)