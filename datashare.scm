
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
(declare (uses margs))
;; (declare (uses dcommon))
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
(define *args-hash* (make-hash-table))
(define datashare:help (conc "Usage: datashare [action [params ...]]

Note: run datashare without parameters to start the gui.

  list-areas                          : List the allowed areas

  list-versions <area>                : List versions available in <area>
         options : -full, -vpatt patt

  publish <path> <area> <version>     : Publish data for area and with version

  get <area> <version>                : Get a link to data, put the link in destpath
         options : -i iteration

  update <area>                       : Update the link to data to the latest iteration.

Part of the Megatest tool suite.
Learn more at http://www.kiatoa.com/fossils/megatest

Version: " megatest-fossil-hash)) ;; "

;;======================================================================
;; RECORDS
;;======================================================================

;; make-vector-record "testing" datastore pkg id area version_name store_type copied source_path iteration submitter datetime storegrp datavol quality disk_id comment
;; testing
(define (make-datashare:pkg)(make-vector 15))
(define-inline (datashare:pkg-get-id             vec)    (vector-ref  vec 0))
(define-inline (datashare:pkg-get-area           vec)    (vector-ref  vec 1))
(define-inline (datashare:pkg-get-version_name   vec)    (vector-ref  vec 2))
(define-inline (datashare:pkg-get-store_type     vec)    (vector-ref  vec 3))
(define-inline (datashare:pkg-get-copied         vec)    (vector-ref  vec 4))
(define-inline (datashare:pkg-get-source_path    vec)    (vector-ref  vec 5))
(define-inline (datashare:pkg-get-iteration      vec)    (vector-ref  vec 6))
(define-inline (datashare:pkg-get-submitter      vec)    (vector-ref  vec 7))
(define-inline (datashare:pkg-get-datetime       vec)    (vector-ref  vec 8))
(define-inline (datashare:pkg-get-storegrp       vec)    (vector-ref  vec 9))
(define-inline (datashare:pkg-get-datavol        vec)    (vector-ref  vec 10))
(define-inline (datashare:pkg-get-quality        vec)    (vector-ref  vec 11))
(define-inline (datashare:pkg-get-disk_id        vec)    (vector-ref  vec 12))
(define-inline (datashare:pkg-get-comment        vec)    (vector-ref  vec 13))
(define-inline (datashare:pkg-get-stored_path    vec)    (vector-ref  vec 14))
(define-inline (datashare:pkg-set-id!            vec val)(vector-set! vec 0 val))
(define-inline (datashare:pkg-set-area!          vec val)(vector-set! vec 1 val))
(define-inline (datashare:pkg-set-version_name!  vec val)(vector-set! vec 2 val))
(define-inline (datashare:pkg-set-store_type!    vec val)(vector-set! vec 3 val))
(define-inline (datashare:pkg-set-copied!        vec val)(vector-set! vec 4 val))
(define-inline (datashare:pkg-set-source_path!   vec val)(vector-set! vec 5 val))
(define-inline (datashare:pkg-set-iteration!     vec val)(vector-set! vec 6 val))
(define-inline (datashare:pkg-set-submitter!     vec val)(vector-set! vec 7 val))
(define-inline (datashare:pkg-set-datetime!      vec val)(vector-set! vec 8 val))
(define-inline (datashare:pkg-set-storegrp!      vec val)(vector-set! vec 9 val))
(define-inline (datashare:pkg-set-datavol!       vec val)(vector-set! vec 10 val))
(define-inline (datashare:pkg-set-quality!       vec val)(vector-set! vec 11 val))
(define-inline (datashare:pkg-set-disk_id!       vec val)(vector-set! vec 12 val))
(define-inline (datashare:pkg-set-comment!       vec val)(vector-set! vec 13 val))
(define-inline (datashare:pkg-set-stored_path!   vec val)(vector-set! vec 14 val))

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

(define (datashare:get-id db area version-name iteration)
  (let ((res #f))
    (sqlite3:for-each-row
     (lambda (id)
       (set! res id))
     db
     "SELECT id FROM pkgs WHERE area=? AND version_name=? AND iteration=?;"
     area version-name iteration)
    res))

(define (datashare:set-stored-path db id path)
  (sqlite3:execute db "UPDATE pkgs SET stored_path=? WHERE id=?;" path id))

(define (datashare:set-copied db id value)
  (sqlite3:execute db "UPDATE pkgs SET copied=? WHERE id=?;" value id))
  
(define (datashare:get-pkg-record db area version-name iteration)
  (let ((res #f))
    (sqlite3:for-each-row
     (lambda (a . b)
       (set! res (apply vector a b)))
     db 
     "SELECT * FROM pkgs WHERE area=? AND version_name=? AND iteration=?;"
     area 
     version-name
     iteration)
    res))

;; take version-name iteration and register or update "lastest/0"
;;
(define (datashare:set-latest db id area version-name iteration)
  (let* ((rec         (datashare:get-pkg-record db area version-name iteration))
	 (latest-id   (datashare:get-id db area "latest" 0))
	 (stored-path (datashare:pkg-get-stored_path rec)))
    (if latest-id ;; have a record - bump the link pointer
	(datashare:set-stored-path db latest-id stored-path)
	(datashare:register-data db area "latest" 'link "auto" "na" stored-path "latest data"))))

;; set a package ref, this is the location where the link back to the stored data 
;; is put. 
;;
;; if there is nothing at that location then the record can be removed
;; if there are no refs for a particular pkg-id then that pkg-id is a 
;; candidate for removal
;;
(define (datashare:record-pkg-ref db pkg-id dest-link)
  (sqlite3:execute db "INSERT INTO refs (pkg_id,destlink) VALUES (?,?);" pkg-id dest-link))
  
(define (datashare:count-refs db pkg-id)
  (let ((res 0))
    (sqlite3:for-each-row
     (lambda (count)
       (set! res count))
     db
     "SELECT count(id) FROM refs WHERE pkg_id=?;"
     pkg-id)
    res))

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
	     (debug:print 2 #f "ERROR: problem accessing db " dbpath
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
        (print-call-chain (current-error-port))
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

(define (datashare:get-pkg db area-name version-name #!key (iteration #f))
  (let ((dat '())
	(res #f))
    (sqlite3:for-each-row ;; replace with fold ...
     (lambda (a . b)
       (set! dat (cons (list->vector (cons a b)) dat)))
     db 
     (conc "SELECT id,area,version_name,store_type,copied,source_path,iteration,submitter,datetime,storegrp,datavol,quality,disk_id,comment,stored_path "
	   " FROM pkgs WHERE area=? AND version_name=? ORDER BY iteration ASC;")
     area-name version-name)
    ;; now filter for iteration, either max if #f or specific one
    (if (null? dat)
	#f
	(let loop ((hed (car dat))
		   (tal (cdr dat))
		   (cur 0))
	  (let ((itr (datashare:pkg-get-iteration hed)))
	    (if (equal? itr iteration) ;; this is the one if iteration is specified
		hed
		(if (null? tal)
		    hed
		    (loop (car tal)(cdr tal)))))))))

(define (datashare:get-versions-for-area db area-name #!key (version-patt #f))
  (let ((res '())
	(data (make-hash-table)))
    (sqlite3:for-each-row
     (lambda (version-name submitter iteration submitted-time comment)
       ;;                                              0           1         2           3           4
       (hash-table-set! data version-name (vector version-name submitter iteration submitted-time comment)))
     db 
     "SELECT version_name,submitter,iteration,datetime,comment FROM pkgs WHERE area='megatest' AND version_name != 'latest' AND version_name LIKE ? ORDER BY datetime asc;"
     (or version-patt "%"))
    (map (lambda (x)(hash-table-ref data x))(sort (hash-table-keys data) string-ci>=))))

;;======================================================================
;; DATA IMPORT/EXPORT
;;======================================================================

(define (datashare:import-data configdat source-path dest-path area version iteration)
  (let* ((space-avail (car dest-path))
	 (disk-path   (cdr dest-path))
	 (targ-path   (conc disk-path "/" area "/" version "/" iteration))
	 (id          (datashare:get-id db area version iteration))
	 (db          (datashare:open-db configdat)))
    (if (> space-avail 10000) ;; dumb heuristic
	(begin
	  (create-directory targ-path #t)
	  (datashare:set-stored-path db id targ-path)
	  (print "Running command: rsync -av " source-path "/ " targ-path "/")
	  (let ((th1 (make-thread (lambda ()
				    (let ((pid (process-run "rsync" (list "-av" (conc source-path "/") (conc targ-path "/")))))
				      (process-wait pid)
				      (datashare:set-copied db id "yes")
				      (sqlite3:finalize! db)))
				   "Data copy")))
	    (thread-start! th1))
	  #t)
	(begin
	  (print "ERROR: Not enough space in storage area " dest-path)
	  (datashare:set-copied db id "no")
	  (sqlite3:finalize! db)
	  #f))))

(define (datashare:get-areas configdat)
  (let* ((areadat (configf:get-section configdat "areas"))
	 (areas   (if areadat (map car areadat) '())))
    areas))

(define (datashare:publish configdat publish-type area-name version comment spath submitter quality)
  ;; input checks
  (cond 
   ((not (member area-name (datashare:get-areas configdat)))
    (cons #f (conc "Illegal area name \"" area-name "\"")))
   (else
    (let ((db          (datashare:open-db configdat))
	  (iteration   (datashare:register-data db area-name version publish-type submitter quality spath comment))
	  (dest-store  (datashare:get-best-storage configdat)))
      (if iteration
	  (if (eq? 'copy publish-type)
	      (begin
		(datashare:import-data configdat spath dest-store area-name version iteration)
		(let ((id (datashare:get-id db area-name version iteration)))
		  (datashare:set-latest db id area-name version iteration)))
	      (let ((id (datashare:get-id db area-name version iteration)))
		(datashare:set-stored-path db id spath)
		(datashare:set-copied db id "yes")
		(datashare:set-copied db id "n/a")
		(datashare:set-latest db id area-name version iteration)))
	  (print "ERROR: Failed to get an iteration number"))
      (sqlite3:finalize! db)
      (cons #t "Successfully saved data")))))

(define (datashare:get-best-storage configdat)
  (let* ((storage     (configf:lookup configdat "settings" "storage"))
	 (store-areas (if storage (string-split storage) '())))
    (print "Looking for available space in " store-areas)
    (datashare:find-most-space store-areas)))

;; (string->number (list-ref (with-input-from-pipe "df -B1000000 /tmp" (lambda ()(read-line)(string-split (read-line)))) 3))

(define (datashare:find-most-space paths)
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
(define (datashare:build-dir-make-link source target)
  (if (file-exists? target)(datashare:backup-move target))
  (create-directory (pathname-directory target) #t)
  (create-symbolic-link source target))

(define (datashare:backup-move path)
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
			  (datashare:publish configdat publish-type area-name version comment spath submitter quality))))
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

(define (datashare:lst->path pathlst)
  (conc "/" (string-intersperse (map conc pathlst) "/")))

(define (datashare:path->lst path)
  (string-split path "/"))

(define (datashare:pathdat-apply-heuristics configdat path)
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
			      (let* ((path   (datashare:lst->path (cdr (tree:node->path obj id))))
				     (record (hash-table-ref/default srcdat path #f)))
				(if record
				    (begin
				      (set! curr-record record)
				      (iup:attribute-set! submitter      "TITLE" (datashare:pkg-get-submitter record))
				      (iup:attribute-set! date-submitted "TITLE" (time->string (seconds->local-time (datashare:pkg-get-datetime record))))
				      (iup:attribute-set! comment        "TITLE" (datashare:pkg-get-comment record))
				      (iup:attribute-set! quality        "TITLE" (datashare:pkg-get-quality record))
				      (iup:attribute-set! copy-link      "TITLE" (datashare:pkg-get-store_type record))
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
			      (let* ((path   (datashare:lst->path (cdr (tree:node->path obj id))))
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
				  (let* ((pkg-path   (list (datashare:pkg-get-area  pkgitem)
							   (datashare:pkg-get-version_name pkgitem)
							   (datashare:pkg-get-iteration pkgitem)))
					 (pkg-id     (datashare:pkg-get-id          pkgitem))
					 (path       (datashare:lst->path pkg-path)))
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
					(tree:add-node tb2 "Installed" (datashare:path->lst path)))
				    (hash-table-set! installed-dat path (datashare:pathdat-apply-heuristics configdat fullpath))))
				areas)
			       (sqlite3:finalize! db))))
	   (apply          (iup:button "Apply"
				       #:action
				       (lambda (obj)
					 (if curr-record
					     (let* ((area        (datashare:pkg-get-area        curr-record))
						    (stored-path (datashare:pkg-get-stored_path curr-record))
						    (source-type (datashare:pkg-get-store_type  curr-record))
						    (source-path (case source-type ;;  (equal? source-type "link"))
								   ((link)(datashare:pkg-get-source-path curr-record))
								   ((copy)stored-path)
								   (else #f)))
						    (dest-stub   (configf:lookup configdat "areas" area))
						    (target-path (conc basepath "/" dest-stub)))
					       (datashare:build-dir-make-link stored-path target-path)
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
;; MISC
;;======================================================================


(define (datashare:do-as-calling-user proc)
  (let ((eid (current-effective-user-id))
        (cid (current-user-id)))
    (if (not (eq? eid cid)) ;; running suid
            (set! (current-effective-user-id) cid))
    ;; (print "running as " (current-effective-user-id))
    (proc)
    (if (not (eq? eid cid))
        (set! (current-effective-user-id) eid))))

(define (datashare:find name paths)
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

(define (datashare:load-config exe-dir exe-name)
  (let* ((fname   (conc exe-dir "/." exe-name ".config")))
    (ini:property-separator-patt " *  *")
    (ini:property-separator #\space)
    (if (file-exists? fname)
	;; (ini:read-ini fname)
	(read-config fname #f #t)
	(make-hash-table))))

(define (datashare:process-action configdat action . args)
  (case (string->symbol action)
    ((get)
     (if (< (length args) 2)
	 (begin 
	   (print "ERROR: Missing arguments; " (string-intersperse args ", "))
	   (exit 1))
	 (let* ((basepath    (configf:lookup configdat "settings" "basepath"))
		(db          (datashare:open-db configdat))
		(area        (car args))
		(version     (cadr args)) ;;    iteration
		(remargs     (args:get-args args '("-i") '() args:arg-hash 0))
		(iteration   (if (args:get-arg "-i")(string->number (args:get-arg "-i")) #f))
		(curr-record (datashare:get-pkg db area version iteration: iteration)))
	   (if (not curr-record)
	       (begin
		 (print "ERROR: No matching record found; area=" area ", version=" version ", iteration=" (if iteration iteration "(max)"))
		 (exit 1))
	       (let* ((stored-path (datashare:pkg-get-stored_path curr-record))
		      (source-type (datashare:pkg-get-store_type  curr-record))
		      (source-path (case source-type ;;  (equal? source-type "link"))
				     ((link) (datashare:pkg-get-source-path curr-record))
				     ((copy) stored-path)
				     (else #f)))
		      (dest-stub   (configf:lookup configdat "areas" area))
		      (target-path (conc basepath "/" dest-stub)))
		 (datashare:build-dir-make-link stored-path target-path)
		 (datashare:record-pkg-ref db (datashare:pkg-get-id curr-record) target-path)
		 (sqlite3:finalize! db)
		 (print "Creating link from " stored-path " to " target-path))))))
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
		(publish-res  (datashare:publish configdat publish-type areaname version comment srcpath submitter quality)))
	   (if (not (car publish-res))
	       (begin
		 (print "ERROR: " (cdr publish-res))
		 (exit 1))))))
    ((list-versions)
     (let ((area-name (car args)) ;;      version patt   full print
	   (remargs   (args:get-args args '("-vpatt") '("-full") args:arg-hash 0))
	   (db        (datashare:open-db configdat))
	   (versions  (datashare:get-versions-for-area db (car args) version-patt: (args:get-arg "-vpatt"))))
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
	    versions)
       (sqlite3:finalize! db)))))

;; ease debugging by loading ~/.dashboardrc - REMOVE FROM PRODUCTION!
(let ((debugcontrolf (conc (get-environment-variable "HOME") "/.datasharerc")))
  (if (file-exists? debugcontrolf)
      (load debugcontrolf)))

(define (main)
  (let* ((args      (argv))
	 (prog      (car args))
	 (rema      (cdr args))
	 (exe-name  (pathname-file (car (argv))))
	 (exe-dir   (or (pathname-directory prog)
			(datashare:find exe-name (string-split (get-environment-variable "PATH") ":"))))
	 (configdat (datashare:load-config exe-dir exe-name)))
    (cond
     ;; one-word commands
     ((eq? (length rema) 1)
      (case (string->symbol (car rema))
	((help -h -help --h --help)
	 (print datashare:help))
	((list-areas)
	 (map print (datashare:get-areas configdat)))
	(else
	 (print "ERROR: Unrecognised command. Try \"datashare help\""))))
     ;; multi-word commands
     ((null? rema)(datashare:gui configdat))
     ((>= (length rema) 2)
      (apply datashare:process-action configdat (car rema)(cdr rema)))
     (else (print "ERROR: Unrecognised command. Try \"datashare help\"")))))

(main)
