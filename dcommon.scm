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

(use format)
(require-library iup)
(import (prefix iup iup:))
(use canvas-draw)

(declare (unit dcommon))

(declare (uses megatest-version))
(declare (uses gutils))
(declare (uses db))
(declare (uses synchash))

(include "common_records.scm")
(include "db_records.scm")
(include "key_records.scm")

;; yes, this is non-ideal 
(define dashboard:update-summary-tab #f)

;;======================================================================
;; D A T A   T A B L E S
;;======================================================================

;; Table of keys
(define (dcommon:keys-matrix rawconfig)
  (let* ((curr-row-num 1)
	 (key-vals     (configf:section-vars rawconfig "fields"))
	 (keys-matrix  (iup:matrix
			#:alignment1 "ALEFT"
			#:expand "YES" ;; "HORIZONTAL" ;; "VERTICAL"
			;; #:scrollbar "YES"
			#:numcol 1
			#:numlin (length key-vals)
			#:numcol-visible 1
			#:numlin-visible (length key-vals)
			#:click-cb (lambda (obj lin col status)
				     (print "obj: " obj " lin: " lin " col: " col " status: " status)))))
    (iup:attribute-set! keys-matrix "0:0" "Run Keys")
    (iup:attribute-set! keys-matrix "0:1" "Key Name")
    ;; (iup:attribute-set! keys-matrix "WIDTH1" "100")
    ;; fill in keys
    (for-each 
     (lambda (var)
       ;; (iup:attribute-set! keys-matrix "ADDLIN" (conc curr-row-num))
       (iup:attribute-set! keys-matrix (conc curr-row-num ":0") curr-row-num)
       (iup:attribute-set! keys-matrix (conc curr-row-num ":1") var)
       (set! curr-row-num (+ 1 curr-row-num))) ;; (config-lookup *configdat* "fields" var)))
     key-vals)
    keys-matrix))

;; Section to table
(define (dcommon:section-matrix rawconfig sectionname varcolname valcolname #!key (title #f))
  (let* ((curr-row-num    1)
	 (key-vals        (configf:section-vars rawconfig sectionname))
	 (section-matrix  (iup:matrix
			   #:alignment1 "ALEFT"
			   #:expand "YES" ;; "HORIZONTAL"
			   #:numcol 1
			   #:numlin (length key-vals)
			   #:numcol-visible 1
			   #:numlin-visible (length key-vals)
			   #:scrollbar "YES")))
    (iup:attribute-set! section-matrix "0:0" varcolname)
    (iup:attribute-set! section-matrix "0:1" valcolname)
    (iup:attribute-set! section-matrix "WIDTH1" "200")
    ;; fill in keys
    (for-each 
     (lambda (var)
       ;; (iup:attribute-set! keys-matrix "ADDLIN" (conc curr-row-num))
       (iup:attribute-set! section-matrix (conc curr-row-num ":0") var)
       (iup:attribute-set! section-matrix (conc curr-row-num ":1") (configf:lookup rawconfig sectionname var))
       (set! curr-row-num (+ 1 curr-row-num))) ;; (config-lookup *configdat* "fields" var)))
     key-vals)
    (iup:vbox
     (iup:label (if title title (conc "Settings from [" sectionname "]"))  
		#:size   "5x"
		#:expand "HORIZONTAL"
		)
     section-matrix)))
    
;; General data
;;
(define (dcommon:general-info)
  (let ((general-matrix (iup:matrix
			 #:alignment1 "ALEFT"
			 #:expand "YES" ;; "HORIZONTAL"
			 #:numcol 1
			 #:numlin 3
			 #:numcol-visible 1
			 #:numlin-visible 3)))
    (iup:attribute-set! general-matrix "WIDTH1" "200")
    (iup:attribute-set! general-matrix "0:1" "About this Megatest area") 
    ;; User (this is not always obvious - it is common to run as a different user
    (iup:attribute-set! general-matrix "1:0" "User")
    (iup:attribute-set! general-matrix "1:1" (current-user-name))
    ;; Megatest area
    (iup:attribute-set! general-matrix "2:0" "Megatest area")
    (iup:attribute-set! general-matrix "2:1" *toppath*)
    ;; Megatest version
    (iup:attribute-set! general-matrix "3:0" "Megatest version")
    (iup:attribute-set! general-matrix "3:1" megatest-version)
    general-matrix))

(define (dcommon:run-stats)
  (let* ((stats-matrix (iup:matrix expand: "YES"))
	 (updater      (lambda ()
			 (let* ((run-stats    (mt:get-run-stats))
				(indices      (common:sparse-list-generate-index run-stats)) ;;  proc: set-cell))
				(max-row      (apply max (map cadr (car indices))))
				(max-col      (apply max (map cadr (cadr indices))))
				(max-visible  (max (- *num-tests* 15) 3))
				(numrows      1)
				(numcols      1)
				(set-cell     (lambda (rnum cnum rname cname v) ;; rownum colnum value
						(print "proc called: " rnum " " cnum " " rname " " cname " " v)
						(if (> rnum numrows)
						    (begin 
						      ;; add rows numrows to r
						      (debug:print 0 "Extending matrix from " numrows " to " rnum)
						      (iup:attribute-set! stats-matrix  "ADDLIN" (conc numrows "-" (- rnum numrows)))
						      (set! numrows rnum)))
						(if (> cnum numcols)
						    (begin 
						      ;; add rows numrows to r
						      (debug:print 0 "Extending matrix from " numcols " to " cnum)
						      (iup:attribute-set! stats-matrix  "ADDLIN" (conc numcols "-" (- rnum numcols)))
						      (set! numcols cnum)))
						(debug:print 0 "Setting row " rnum ", col " cnum " to " v)
						(iup:attribute-set! stats-matrix (conc rnum ":" cnum) v)))
				(row-indices  (car indices))
				(col-indices  (cadr indices)))
			   (iup:attribute-set! stats-matrix "NUMCOL" max-col )
			   (iup:attribute-set! stats-matrix "NUMLIN" (if (< max-row max-visible) max-visible max-row)) ;; min of 20
			   (iup:attribute-set! stats-matrix "NUMCOL_VISIBLE" max-col)
			   (iup:attribute-set! stats-matrix "NUMLIN_VISIBLE" (if (> max-row max-visible) max-visible max-row))

			   ;; Row labels
			   (for-each (lambda (ind)
				       (let ((name (car ind))
					     (num  (cadr ind)))
					 (iup:attribute-set! stats-matrix (conc num ":0") name)))
				     row-indices)

			   ;; Col labels
			   (for-each (lambda (ind)
				       (let ((name (car ind))
					     (num  (cadr ind)))
					 (iup:attribute-set! stats-matrix (conc "0:" num) name)))
				     col-indices)

			   ;; Cell contents
			   (for-each (lambda (entry)
				       (let* ((row-name (car entry))
					      (col-name (cadr entry))
					      (value    (caddr entry))
					      (row-num  (cadr (assoc row-name row-indices)))
					      (col-num  (cadr (assoc col-name col-indices))))
					 (iup:attribute-set! stats-matrix (conc row-num ":" col-num) value)))
				     run-stats)
			   (iup:attribute-set! stats-matrix "REDRAW" "ALL")))))
    (updater)
    (set! dashboard:update-summary-tab updater)
    (iup:attribute-set! stats-matrix "WIDTHDEF" "40")
    (iup:vbox
     (iup:label "Run statistics"  #:expand "HORIZONTAL")
     stats-matrix)))

;; The main menu
(define (dcommon:main-menu)
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

