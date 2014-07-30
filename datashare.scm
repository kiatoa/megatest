
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
;; DB
;;======================================================================

(define (datashare:initialize-db db)
  (for-each
   (lambda (qry)
     (sqlite3:execute db qry))
   (list 
    "CREATE TABLE pkgs 
         (id        INTEGER PRIMARY KEY,
          area      TEXT,
          key       TEXT,
          iteration INTEGER,
          submitter TEXT,
          datetime  TEXT,
          storegrp  TEXT,
          disk_id   INTEGER,
          comment   TEXT);"
    "CREATE TABLE refs
         (id        INTEGER PRIMARY KEY,
          pkg_id    INTEGER,
          destlink  TEXT);"
    "CREATE TABLE disks
         (id         INTEGER PRIMARY KEY,
          storegrp   TEXT,
          path       TEXT);")))

;; Create the sqlite db
(define (datashare:open-db path) 
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
	db)))

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

(define (datashare:publish-view)
  (iup:vbox
   (iup:hbox 
    (iup:button "Pushme" 
		#:expand "YES"
		))))

(define (datashare:get-view)
  (iup:vbox
   (iup:hbox 
    (iup:button "Pushme"
		#:expand "YES"
		))))

(define (datashare:manage-view)
  (iup:vbox
   (iup:hbox 
    (iup:button "Pushme"
		#:expand "YES"
		))))

(define (datashare:gui)
  (iup:show
   (iup:dialog 
    #:title (conc "DataShare dashboard " (current-user-name) ":" (current-directory))
    #:menu (datashare:main-menu)
    (let* ((tabs (iup:tabs
		  #:tabchangepos-cb (lambda (obj curr prev)
				      (set! *datashare:current-tab-number* curr))
		  (datashare:publish-view)
		  (datashare:get-view)
		  (datashare:manage-view)
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
  (let ((fname (conc path "/.datashare.config")))
    (ini:property-separator-patt " *  *")
    (ini:property-separator #\space)
    (if (file-exists? fname)
	(ini:read fname)
	'())))

(define (main)
  (let* ((args (argv))
	 (prog (car args))
	 (rema (cdr args))
	 (conf (datashare:load-config (pathname-directory prog))))
    (cond
     ((eq? (length rema) 1)
      (case (string->symbol (car rema))
	((help -h -help --h --help)
	 (print datashare:help))
	(else
	 (print "ERROR: Unrecognised command. Try \"datashare help\""))))
     ((null? rema)(datashare:gui))
     ((>= (length rema) 2)
      (apply process-action (car rema)(cdr rema)))
     (else (print "ERROR: Unrecognised command. Try \"datashare help\"")))))

(main)