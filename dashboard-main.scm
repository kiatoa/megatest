;;======================================================================
;; Copyright 2006-2011, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.
;;======================================================================

;;======================================================================
;; Main Megatest Panel
;;======================================================================

(use format)
(require-library iup)
(import (prefix iup iup:))

(use canvas-draw)

(use sqlite3 srfi-1 posix regex regex-case srfi-69)
(import (prefix sqlite3 sqlite3:))

(declare (unit dashboard-guimonitor))
(declare (uses common))
(declare (uses keys))
(declare (uses db))
(declare (uses tasks))

(include "common_records.scm")
(include "db_records.scm")
(include "run_records.scm")
(include "task_records.scm")

(define (main-menu)
  (menu ;; a menu is a special attribute to a dialog (think Gnome putting the menu at screen top)
   (menu-item "Files" (menu   ;; Note that you can use either #:action or action: for options
		       (menu-item "Open"  action: (lambda (obj)
						    (show (file-dialog))
						    (print "File->open " obj)))
		       (menu-item "Save"  #:action (lambda (obj)(print "File->save " obj)))
		       (menu-item "Exit"  #:action (lambda (obj)(exit)))))
   (menu-item "Tools" (menu
		       (menu-item "Create new blah" #:action (lambda (obj)(print "Tools->new blah")))
		       (menu-item "Show dialog"     #:action (lambda (obj)
							       (show message-window
								     #:modal? #t
								     ;; set positon using coordinates or center, start, top, left, end, bottom, right, parent-center, current
								     ;; #:x 'mouse
								     ;; #:y 'mouse
								     )))))))
(define (main-panel mtest rconfig tests runs)
  (dialog
   #:title "Menu Test"
   #:menu (main-menu)
   (let ((tabtop (iup:tabs mtest rconfig tests runs)))
     (iup:attribute-set! tabtop "TABTITLE0" "Megatest") 
     (iup:attribute-set! tabtop "TABTITLE1" "Runconfigs")
     (iup:attribute-set! tabtop "TABTITLE2" "Tests")
     (iup:attribute-set! tabtop "TABTITLE3" "Runs")
     tabtop)))