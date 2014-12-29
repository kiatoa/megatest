;;======================================================================
;; Copyright 2006-2013, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.
;;======================================================================

(use format numbers)
(require-library iup)
(import (prefix iup iup:))
(use canvas-draw)

(use sqlite3 srfi-1 posix regex regex-case srfi-69)
(import (prefix sqlite3 sqlite3:))

(declare (uses margs))
(declare (uses launch))
(declare (uses megatest-version))
(declare (uses gutils))
(declare (uses db))
(declare (uses server))
(declare (uses synchash))
(declare (uses dcommon))
(declare (uses tree))

(include "common_records.scm")
(include "db_records.scm")
(include "key_records.scm")

(define help (conc 
"Megatest Dashboard, documentation at http://www.kiatoa.com/fossils/megatest
  version " megatest-version "
  license GPL, Copyright (C) Matt Welland 2011

Usage: dashboard [options]
  -h                : this help
  -server host:port : connect to host:port instead of db access
  -test testid      : control test identified by testid
  -guimonitor       : control panel for runs

Misc
  -rows N         : set number of rows
"))

;; process args
(define remargs (args:get-args 
		 (argv)
		 (list  "-rows"
			"-run"
			"-test"
			"-debug"
			"-host" 
			) 
		 (list  "-h"
			"-guimonitor"
			"-main"
			"-v"
			"-q"
		       )
		 args:arg-hash
		 0))

(if (args:get-arg "-h")
    (begin
      (print help)
      (exit)))

;; ease debugging by loading ~/.dashboardrc
(let ((debugcontrolf (conc (get-environment-variable "HOME") "/.dashboardrc")))
  (if (file-exists? debugcontrolf)
      (load debugcontrolf)))

(debug:setup)

(define *tim* (iup:timer))
(define *ord* #f)

(iup:attribute-set! *tim* "TIME" 300)
(iup:attribute-set! *tim* "RUN" "YES")

(define (message-window msg)
  (iup:show
   (iup:dialog
    (iup:vbox 
     (iup:label msg #:margin "40x40")))))

(define (iuplistbox-fill-list lb items . default)
  (let ((i 1)
	(selected-item (if (null? default) #f (car default))))
    (iup:attribute-set! lb "VALUE" (if selected-item selected-item ""))
    (for-each (lambda (item)
		(iup:attribute-set! lb (number->string i) item)
		(if selected-item
		    (if (equal? selected-item item)
			(iup:attribute-set! lb "VALUE" item))) ;; (number->string i))))
		(set! i (+ i 1)))
	      items)
    i))

(define (pad-list l n)(append l (make-list (- n (length l)))))

(define (mkstr . x)
  (string-intersperse (map conc x) ","))

;;======================================================================
;; G L O B A L   D A T A   S T R U C T U R E ? ?
;;======================================================================

;; testsuite nick-> [ path
;;                    linktree
;;                    target/runname -> [ id (ht) -> [ runname
;;                                                     tests -> [ id (ht) -> state
;;                                                                           status
;;                                                                           item_path
;;                                                                           fulldat -> [ remaining data ]

;;======================================================================
;; T E S T S
;;======================================================================

(define (tree-path->test-id path)
  (if (not (null? path))
      (hash-table-ref/default (dboard:data-get-path-test-ids *data*) path #f)
      #f))



;;======================================================================
;; R U N   C O N T R O L
;;======================================================================


;; Browse and control a single run
;;
(define (runcontrol window-id)
  (iup:hbox))

;;======================================================================
;; D A S H B O A R D
;;======================================================================

;; Main Panel
(define (main-panel) ;;  window-id)
  (iup:dialog
   #:title "Megatest Control Panel"
   #:menu (dcommon:main-menu)
   #:shrink "YES"
   (let ((tabtop (iup:tabs)))
     tabtop)))

(define *current-window-id* 0)

(define (newdashboard dbstruct)
  (let* ((data     (make-hash-table))
	 (keys     (db:get-keys dbstruct))
	 (runname  "%")
	 (testpatt "%")
	 (keypatts (map (lambda (k)(list k "%")) keys))
	 (states   '())
	 (statuses '())
	 (nextmintime (current-milliseconds))
	 (my-window-id *current-window-id*))
    (set! *current-window-id* (+ 1 *current-window-id*))
    (dboard:data-set-runs! *data* data) ;; make this data available to the rest of the application
    (iup:show (main-panel my-window-id))
    ;; Yes, running iup:show will pop up a new panel
    ;; (iup:show (main-panel my-window-id))
    (iup:callback-set! *tim*
		       "ACTION_CB"
		       (lambda (x)
			 ;; Want to dedicate no more than 50% of the time to this so skip if
			 ;; 2x delta time has not passed since last query
			 (if (< nextmintime (current-milliseconds))
			     (let* ((starttime (current-milliseconds))
				    (changes   (dcommon:run-update keys data runname keypatts testpatt states statuses 'full my-window-id))
				    (endtime   (current-milliseconds)))
			       (set! nextmintime (+ endtime (* 2 (- endtime starttime))))
			       (debug:print 11 "CHANGE(S): " (car changes) "..."))
			     (debug:print-info 11 "Server overloaded"))))))

(dboard:data-set-updaters! *data* (make-hash-table))
(iup:show (main-panel))
(iup:main-loop)
