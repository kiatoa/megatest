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

(use sqlite3 srfi-1 posix regex-case base64 format dot-locking csv-xml)
(require-extension sqlite3 regex posix)

(require-extension (srfi 18) extras tcp rpc)

(import (prefix sqlite3 sqlite3:))
(import (prefix base64 base64:))

(declare (unit common))

(include "common_records.scm")

;; (require-library margs)
;; (include "margs.scm")

(define getenv get-environment-variable)

(define home (getenv "HOME"))
(define user (getenv "USER"))

;; global gletches
(define *configinfo* #f)
(define *configdat*  #f)
(define *toppath*    #f)
(define *already-seen-runconfig-info* #f)
(define *waiting-queue*     (make-hash-table))
(define *test-meta-updated* (make-hash-table))
(define *globalexitstatus*  0) ;; attempt to work around possible thread issues
(define *passnum*           0) ;; when running track calls to run-tests or similar
(define *verbosity*         1)
(define *rpc:listener*      #f) ;; if set up for server communication this will hold the tcp port
(define *runremote*         #f) ;; if set up for server communication this will hold <host port>
(define *last-db-access*    0)  ;; update when db is accessed via server
(define *target*            #f) ;; cache the target here; target is keyval1/keyval2/.../keyvalN

(define (get-with-default val default)
  (let ((val (args:get-arg val)))
    (if val val default)))

(define (assoc/default key lst . default)
  (let ((res (assoc key lst)))
    (if res (cadr res)(if (null? default) #f (car default)))))

;;======================================================================
;; Misc utils
;;======================================================================

;; convert stuff to a number if possible
(define (any->number val)
  (cond 
   ((number? val) val)
   ((string? val) (string->number val))
   ((symbol? val) (any->number (symbol->string val)))
   (else #f)))

(define (any->number-if-possible val)
  (let ((num (any->number val)))
    (if num num val)))

(define (patt-list-match item patts)
  (if (and item patts)  ;; here we are filtering for matches with -itempatt
      (let ((res #f))   ;; look through all the item-patts if defined, format is patt1,patt2,patt3 ... wildcard is %
	(for-each 
	 (lambda (patt)
	   (if (string-match (glob->regexp
			       (string-translate patt "%" "*"))
			      item)
	       (set! res #t)))
	 (string-split patts ","))
	res)
      #t))

;;======================================================================
;; System stuff
;;======================================================================

(define (get-df path)
  (let* ((df-results (cmd-run->list (conc "df " path)))
	 (space-rx   (regexp "([0-9]+)\\s+([0-9]+)%"))
	 (freespc    #f))
    ;; (write df-results)
    (for-each (lambda (l)
		(let ((match (string-search space-rx l)))
		  (if match 
		      (let ((newval (string->number (cadr match))))
			(if (number? newval)
			    (set! freespc newval))))))
	      (car df-results))
    freespc))
  
(define (get-cpu-load)
  (let* ((load-res (cmd-run->list "uptime"))
	 (load-rx  (regexp "load average:\\s+(\\d+)"))
	 (cpu-load #f))
    (for-each (lambda (l)
		(let ((match (string-search load-rx l)))
		  (if match
		      (let ((newval (string->number (cadr match))))
			(if (number? newval)
			    (set! cpu-load newval))))))
	      (car load-res))
    cpu-load))

(define (get-uname . params)
  (let* ((uname-res (cmd-run->list (conc "uname " (if (null? params) "-a" (car params)))))
	 (uname #f))
    (if (null? (car uname-res))
	"unknown"
	(caar uname-res))))
	      
(define (save-environment-as-files fname)
  (let ((envvars (get-environment-variables))
        (whitesp (regexp "[^a-zA-Z0-9_\\-:;,.\\/%$]")))
     (with-output-to-file (conc fname ".csh")
       (lambda ()
          (for-each (lambda (key)
                      (let* ((val (cdr key))
                             (sval (if (string-search whitesp val)(conc "\"" val "\"") val)))
                        (print "setenv " (car key) " " sval)))
                     envvars)))
     (with-output-to-file (conc fname ".sh")
       (lambda ()
          (for-each (lambda (key)
                      (let* ((val (cdr key))
                             (sval (if (string-search whitesp val)(conc "\"" val "\"") val)))
                         (print "export " (car key) "=" sval)))
                    envvars)))))

;; set some env vars from an alist, return an alist with original values
;; (("VAR" "value") ...)
(define (alist->env-vars lst)
  (if (list? lst)
      (let ((res '()))
	(for-each (lambda (p)
		    (let* ((var (car  p))
			   (val (cadr p))
			   (prv (get-environment-variable var)))
		      (set! res (cons (list var prv) res))
		      (if val 
			  (setenv var (->string val))
			  (unsetenv var))))
		  lst)
	res)
      '()))
		  
;;======================================================================
;; time and date nice to have stuff
;;======================================================================

(define (seconds->hr-min-sec secs)
  (let* ((hrs (quotient secs 3600))
	 (min (quotient (- secs (* hrs 3600)) 60))
	 (sec (- secs (* hrs 3600)(* min 60))))
    (conc (if (> hrs 0)(conc hrs "hr ") "")
	  (if (> min 0)(conc min "m ")  "")
	  sec "s")))

(define (seconds->time-string sec)
  (time->string 
   (seconds->local-time sec) "%H:%M:%S"))

;;======================================================================
;; Colors
;;======================================================================
      
(define (common:name->iup-color name)
  (case (string->symbol (string-downcase name))
    ((red)    "223 33 49")
    ((grey)   "192 192 192")
    ((orange) "255 172 13")
    ((purple) "This is unfinished ...")))

(define (common:get-color-for-state-status state status type)
  (case (string->symbol state)
    ((COMPLETED)
     (if (equal? status "PASS")
	 "70 249 73"
	 (if (or (equal? status "WARN")
		 (equal? status "WAIVED"))
	     "255 172 13"
	     "223 33 49"))) ;; greenish orangeish redish
    ((LAUNCHED)         "101 123 142")
    ((CHECK)            "255 100 50")
    ((REMOTEHOSTSTART)  "50 130 195")
    ((RUNNING)          "9 131 232")
    ((KILLREQ)          "39 82 206")
    ((KILLED)           "234 101 17")
    ((NOT_STARTED)      "240 240 240")
    (else               "192 192 192")))

(define (common:get-color-from-status status)
  (cond
   ((equal? status "PASS")    "green")
   ((equal? status "FAIL")    "red")
   ((equal? status "WARN")    "orange")
   ((equal? status "KILLED")  "orange")
   ((equal? status "KILLREQ") "purple")
   ((equal? status "RUNNING") "blue")
   (else "black")))
