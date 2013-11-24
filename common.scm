;;======================================================================
;; Copyright 2006-2012, Matthew Welland.
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

;; GLOBAL GLETCHES
(define *db-keys* #f)
(define *configinfo* #f)
(define *configdat*  #f)
(define *toppath*    #f)
(define *already-seen-runconfig-info* #f)
(define *waiting-queue*     (make-hash-table))
(define *test-meta-updated* (make-hash-table))
(define *globalexitstatus*  0) ;; attempt to work around possible thread issues
(define *passnum*           0) ;; when running track calls to run-tests or similar

;; DATABASE
(define *open-dbs* (vector #f (make-hash-table))) ;; megatestdb run-id-dbs

;; SERVER
(define *my-client-signature* #f)
(define *transport-type*    'fs)
(define *megatest-db*       #f)
(define *rpc:listener*      #f) ;; if set up for server communication this will hold the tcp port
(define *runremote*         #f) ;; if set up for server communication this will hold <host port>
(define *last-db-access*    (current-seconds))  ;; update when db is accessed via server
(define *max-cache-size*    0)
(define *logged-in-clients* (make-hash-table))
(define *client-non-blocking-mode* #f)
(define *server-id*         #f)
(define *server-info*       #f)
(define *time-to-exit*      #f)
(define *received-response* #f)
(define *default-numtries*  10)
(define *server-run*        #t)
(define *db-write-access*   #t)
(define *inmemdb*           #f)

(define *target*            (make-hash-table)) ;; cache the target here; target is keyval1/keyval2/.../keyvalN
(define *keys*              (make-hash-table)) ;; cache the keys here
(define *keyvals*           (make-hash-table))
(define *toptest-paths*     (make-hash-table)) ;; cache toptest path settings here
(define *test-paths*        (make-hash-table)) ;; cache test-id to test run paths here
(define *test-ids*          (make-hash-table)) ;; cache run-id, testname, and item-path => test-id
(define *test-info*         (make-hash-table)) ;; cache the test info records, update the state, status, run_duration etc. from testdat.db

(define *run-info-cache*    (make-hash-table)) ;; run info is stable, no need to reget

;; Awful. Please FIXME
(define *env-vars-by-run-id* (make-hash-table))
(define *current-run-name*   #f)

;; Testconfig and runconfig caches. 
(define *testconfigs*       (make-hash-table)) ;; test-name => testconfig
(define *runconfigs*        (make-hash-table)) ;; target    => runconfig

;; This is a cache of pre-reqs met, don't re-calc in cases where called with same params less than
;; five seconds ago
(define *pre-reqs-met-cache* (make-hash-table))

(define (common:clear-caches)
  (set! *target*             (make-hash-table))
  (set! *keys*               (make-hash-table))
  (set! *keyvals*            (make-hash-table))
  (set! *toptest-paths*      (make-hash-table))
  (set! *test-paths*         (make-hash-table))
  (set! *test-ids*           (make-hash-table))
  (set! *test-info*          (make-hash-table))
  (set! *run-info-cache*     (make-hash-table))
  (set! *env-vars-by-run-id* (make-hash-table))
  (set! *test-id-cache*      (make-hash-table)))

;;======================================================================
;; S T A T E S   A N D   S T A T U S E S
;;======================================================================

(define *common:std-states*   
  '((0 "COMPLETED")
    (1 "NOT_STARTED")
    (2 "RUNNING")
    (3 "REMOTEHOSTSTART")
    (4 "LAUNCHED")
    (5 "KILLED")
    (6 "KILLREQ")
    (7 "STUCK")))

(define *common:std-statuses*
  '((0 "PASS")
    (1 "WARN")
    (2 "FAIL")
    (3 "CHECK")
    (4 "n/a")
    (5 "WAIVED")
    (6 "SKIP")
    (7 "DELETED")
    (8 "STUCK/DEAD")))

;; These are stopping conditions that prevent a test from being run
(define *common:cant-run-states-sym* 
  '(COMPLETED KILLED WAIVED UNKNOWN INCOMPLETE))

;;======================================================================
;; D E B U G G I N G   S T U F F 
;;======================================================================

(define *verbosity*         1)
(define *logging*           #f)

(define (get-with-default val default)
  (let ((val (args:get-arg val)))
    (if val val default)))

(define (assoc/default key lst . default)
  (let ((res (assoc key lst)))
    (if res (cadr res)(if (null? default) #f (car default)))))

;;======================================================================
;; Misc utils
;;======================================================================

;; Convert strings like "5s 2h 3m" => 60x60x2 + 3x60 + 5
(define (common:hms-string->seconds tstr)
  (let ((parts     (string-split tstr))
	(time-secs 0)
	;; s=seconds, m=minutes, h=hours, d=days
	(trx       (regexp "(\\d+)([smhd])")))
    (for-each (lambda (part)
		(let ((match  (string-match trx part)))
		  (if match
		      (let ((val (string->number (cadr match)))
			    (unt (caddr match)))
			(if val 
			    (set! time-secs (+ time-secs (* val
							    (case (string->symbol unt)
							      ((s) 1)
							      ((m) 60)
							      ((h) (* 60 60))
							      ((d) (* 24 60 60))
							      (else 0))))))))))
	      parts)
    time-secs))
		       
(define (common:version-signature)
  (conc megatest-version "-" (substring megatest-fossil-hash 0 4)))

;; one-of args defined
(define (args-defined? . param)
  (let ((res #f))
    (for-each 
     (lambda (arg)
       (if (args:get-arg arg)(set! res #t)))
     param)
    res))

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
  (debug:print-info 8 "patt-list-match item=" item " patts=" patts)
  (if (and item patts)  ;; here we are filtering for matches with item patterns
      (let ((res #f))   ;; look through all the item-patts if defined, format is patt1,patt2,patt3 ... wildcard is %
	(for-each 
	 (lambda (patt)
	   (let ((modpatt (string-substitute "%" ".*" patt #t)))
	     (debug:print-info 10 "patt " patt " modpatt " modpatt)
	     (if (string-match (regexp modpatt) item)
		 (set! res #t))))
	 (string-split patts ","))
	res)
      #t))

;; (map print (map car (hash-table->alist (read-config "runconfigs.config" #f #t))))
(define (common:get-runconfig-targets)
  (sort (map car (hash-table->alist
		  (read-config "runconfigs.config"
			       #f #t))) string<?))

;; '(print (string-intersperse (map cadr (hash-table-ref/default (read-config "megatest.config" \#f \#t) "disks" '"'"'("none" ""))) "\n"))'
(define (common:get-disks)
  (hash-table-ref/default 
   (read-config "megatest.config" #f #t)
   "disks" '("none" "")))

;;======================================================================
;; M I S C   L I S T S
;;======================================================================

;; items in lista are matched value and position in listb
;; return the remaining items in listb or #f
;;
(define (common:list-is-sublist lista listb)
  (if (null? lista)
      listb ;; all items in listb are "remaining"
      (if (> (length lista)(length listb)) 
	  #f
	  (let loop ((heda (car lista))
		     (tala (cdr lista))
		     (hedb (car listb))
		     (talb (cdr listb)))
	    (if (equal? heda hedb)
		(if (null? tala) ;; we are done
		    talb
		    (loop (car tala)
			  (cdr tala)
			  (car talb)
			  (cdr talb)))
		#f)))))

;; Needed for long lists to be sorted where (apply max ... ) dies
;;
(define (common:max inlst)
  (let loop ((max-val (car inlst))
	     (hed     (car inlst))
	     (tal     (cdr inlst)))
    (if (not (null? tal))
	(loop (max hed max-val)
	      (car tal)
	      (cdr tal))
	(max hed max-val))))


;;======================================================================
;; Munge data into nice forms
;;======================================================================

;; Generate an index for a sparse list of key values
;;   ( (rowname1 colname1 val1)(rowname2 colname2 val2) )
;;
;; => 
;;
;;   ( (rowname1 0)(rowname2 1))    ;; rownames -> num
;;     (colname1 0)(colname2 1)) )  ;; colnames -> num
;; 
;; optional apply proc to rownum colnum value
(define (common:sparse-list-generate-index data #!key (proc #f))
  (if (null? data)
      (list '() '())
      (let loop ((hed (car data))
		 (tal (cdr data))
		 (rownames '())
		 (colnames '())
		 (rownum   0)
		 (colnum   0))
	(let* ((rowkey          (car   hed))
	       (colkey          (cadr  hed))
	       (value           (caddr hed))
	       (existing-rowdat (assoc rowkey rownames))
	       (existing-coldat (assoc colkey colnames))
	       (curr-rownum     (if existing-rowdat rownum (+ rownum 1)))
	       (curr-colnum     (if existing-coldat colnum (+ colnum 1)))
	       (new-rownames    (if existing-rowdat rownames (cons (list rowkey curr-rownum) rownames)))
	       (new-colnames    (if existing-coldat colnames (cons (list colkey curr-colnum) colnames))))
	  ;; (debug:print-info 0 "Processing record: " hed )
	  (if proc (proc curr-rownum curr-colnum rowkey colkey value))
	  (if (null? tal)
	      (list new-rownames new-colnames)
	      (loop (car tal)
		    (cdr tal)
		    new-rownames
		    new-colnames
		    (if (> curr-rownum rownum) curr-rownum rownum)
		    (if (> curr-colnum colnum) curr-colnum colnum)
		    ))))))

;;======================================================================
;; System stuff
;;======================================================================

;; return a nice clean pathname made absolute
(define (nice-path dir)
  (normalize-pathname (if (absolute-pathname? dir)
			  dir
			  (conc (current-directory) "/" dir))))

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
	      
(define (save-environment-as-files fname #!key (ignorevars (list "DISPLAY" "LS_COLORS" "XKEYSYMDB" "EDITOR")))
  (let ((envvars (get-environment-variables))
        (whitesp (regexp "[^a-zA-Z0-9_\\-:;,.\\/%$]")))
     (with-output-to-file (conc fname ".csh")
       (lambda ()
          (for-each (lambda (key)
		      (if (not (member key ignorevars))
			  (let* ((val (cdr key))
				 (sval (if (string-search whitesp val)(conc "\"" val "\"") val)))
			    (print "setenv " (car key) " " sval))))
		      envvars)))
     (with-output-to-file (conc fname ".sh")
       (lambda ()
          (for-each (lambda (key)
		      (if (not (member key ignorevars))
			  (let* ((val (cdr key))
				 (sval (if (string-search whitesp val)(conc "\"" val "\"") val)))
			    (print "export " (car key) "=" sval))))
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

(define (seconds->work-week/day-time sec)
  (time->string
   (seconds->local-time sec) "ww%V.%u %H:%M"))

(define (seconds->work-week/day sec)
  (time->string
   (seconds->local-time sec) "ww%V.%u"))

(define (seconds->year-work-week/day sec)
  (time->string
   (seconds->local-time sec) "%yww%V.%w"))

(define (seconds->year-work-week/day-time sec)
  (time->string
   (seconds->local-time sec) "%yww%V.%w %H:%M"))

;;======================================================================
;; Colors
;;======================================================================
      
(define (common:name->iup-color name)
  (case (string->symbol (string-downcase name))
    ((red)    "223 33 49")
    ((grey)   "192 192 192")
    ((orange) "255 172 13")
    ((purple) "This is unfinished ...")))

;; (define (common:get-color-for-state-status state status)
;;   (case (string->symbol state)
;;     ((COMPLETED)
;;      (case (string->symbol status)
;;        ((PASS)        "70  249 73")
;;        ((WARN WAIVED) "255 172 13")
;;        ((SKIP)        "230 230 0")
;;        (else "223 33 49")))
;;     ((LAUNCHED)         "101 123 142")
;;     ((CHECK)            "255 100 50")
;;     ((REMOTEHOSTSTART)  "50  130 195")
;;     ((RUNNING)          "9   131 232")
;;     ((KILLREQ)          "39  82  206")
;;     ((KILLED)           "234 101 17")
;;     ((NOT_STARTED)      "240 240 240")
;;     (else               "192 192 192")))

(define (common:get-color-from-status status)
  (cond
   ((equal? status "PASS")    "green")
   ((equal? status "FAIL")    "red")
   ((equal? status "WARN")    "orange")
   ((equal? status "KILLED")  "orange")
   ((equal? status "KILLREQ") "purple")
   ((equal? status "RUNNING") "blue")
   (else "black")))
