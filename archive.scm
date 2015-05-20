;; Copyright 2006-2014, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

;;  strftime('%m/%d/%Y %H:%M:%S','now','localtime')

(use sqlite3 srfi-1 posix regex regex-case srfi-69 dot-locking format md5 message-digest)
(import (prefix sqlite3 sqlite3:))

(declare (unit archive))
(declare (uses db))
(declare (uses common))

(include "common_records.scm")
(include "db_records.scm")

;;======================================================================
;; 
;;======================================================================

;; NOT CURRENTLY USED
;;
(define (archive:main linktree target runname testname itempath options)
  (let ((testdir (conc linktree "/" target "/" runname "/" testname "/" itempatt))
	(flavor  'plain) ;; type of machine to run jobs on
	(maxload 1.5)   ;; max allowed load for this work
	(adisks  (archive:get-archive-disks)))
    ;; get testdir size
    ;;   - hand off du to job mgr
    (if (and (file-exists? testdir)
	     (file-is-writable? testdir))
	(let* ((dused  (jobrunner:run-job 
			flavor  ;; machine type
			maxload ;; max allowed load
			'()     ;; prevars - environment vars to set for the job
			common:get-disk-space-used  ;; if a proc call it, if a string it is a unix command
			(list testdir)))
	       (apath  (archive:get-archive testname itempath dused)))
	  (jobrunner:run-job
	   flavor
	   maxload
	   '()
	   archive:run-bup ;; this will break!!! need area-dat
	   (list testdir apath))))))
	  
;; Get archive disks from megatest.config
;;
(define (archive:get-archive-disks area-dat)
  (let ((section (configf:get-section (megatest:area-configdat area-dat) "archive-disks")))
    (if section
	section
	'())))

;; look for the best candidate archive area, else create new 
;; area
;;
(define (archive:get-archive testname itempath dused)
  ;; look up in archive_allocations if there is a pre-used archive
  ;; with adequate diskspace
  ;;
  (let* ((existing-blocks (rmt:archive-get-allocations testname itempath dused))
	 (candidate-disks (map (lambda (block)
				 (list
				  (vector-ref block 1)   ;; archive-area-name
				  (vector-ref block 2))) ;; disk-path
			       existing-blocks)))
    (or (common:get-disk-with-most-free-space candidate-disks dused)
	(archive:allocate-new-archive-block testname itempath))))

;; allocate a new archive area
;;
(define (archive:allocate-new-archive-block run-area-home testsuite-name dneeded)
  (let* ((adisks    (archive:get-archive-disks))
	 (best-disk (common:get-disk-with-most-free-space adisks dneeded)))
    (if best-disk
	(let* ((bdisk-name    (car best-disk))
	       (bdisk-path    (cdr best-disk))
	       (area-key      (substring (message-digest-string (md5-primitive) run-area-home) 0 5))
	       (bdisk-id      (rmt:archive-register-disk bdisk-name bdisk-path (get-df bdisk-path)))
	       (archive-name  (let ((sec (current-seconds)))
				(conc (time->string (seconds->local-time sec) "%Y")
				      "_q" (seconds->quarter sec) "/"
				      testsuite-name "_" area-key)))
	       (archive-path  (conc bdisk-path "/" archive-name))
	       (block-id      (rmt:archive-register-block-name bdisk-id archive-path)))
	  ;;   (allocation-id (rmt:archive-allocate-testsuite/area-to-block block-id testsuite-name area-key)))
	  (if block-id ;; (and block-id allocation-id)
	      (cons block-id archive-path)
	      #f))
	#f)))

;; archive - run bup
;;
;; 1. create the bup dir if not exists
;; 2. start the du of each directory
;; 3. gen index
;; 4. save
;;
(define (archive:run-bup archive-command run-id run-name tests area-dat)
  ;; move the getting of archive space down into the below block so that a single run can 
  ;; allocate as needed should a disk fill up
  ;;
  (let* ((configdat    (megatest:area-configdat area-dat))
	 (toppath      (megatest:area-path      area-dat))
	 (min-space    (string->number (or (configf:lookup configdat "archive" "minspace") "1000")))
	 (archive-info (archive:allocate-new-archive-block toppath (common:get-testsuite-name) min-space))
	 (archive-dir  (if archive-info (cdr archive-info) #f))
	 (archive-id   (if archive-info (car archive-info) -1))
	 (disk-groups  (make-hash-table))
	 (test-groups  (make-hash-table)) ;; these two (disk and test groups) could be combined nicely
	 (bup-exe      (or (configf:lookup configdat "archive" "bup") "bup"))
	 (compress     (or (configf:lookup configdat "archive" "compress") "9"))
	 (linktree     (configf:lookup configdat "setup" "linktree")))

    (if (not archive-dir) ;; no archive disk found, this is fatal
	(begin
	  (debug:print 0 "FATAL: No archive disks found. Please add disks with at least " min-space " MB space to the [archive-disks] section of megatest.config")
	  (debug:print 0 "       use [archive] minspace to specify minimum available space")
	  (debug:print 0 "   disks: " (string-intersperse (map cadr (archive:get-archive-disks)) "\n         "))
	  (exit 1))
	(debug:print-info 0 "Using path " archive-dir " for archiving"))

    ;; from the test info bin the path to the test by stem
    ;;
    (for-each
     (lambda (test-dat)
       (let* ((item-path         (db:test-get-item-path test-dat))
	      (test-name         (db:test-get-testname  test-dat))
	      (test-id           (db:test-get-id        test-dat))
	      (run-id            (db:test-get-run_id    test-dat))
	      (target            (string-intersperse (map cadr (rmt:get-key-val-pairs run-id)) "/"))
	      
	      (toplevel/children (and (db:test-get-is-toplevel test-dat)
				      (> (rmt:test-toplevel-num-items run-id test-name) 0)))
	      (test-partial-path (conc target "/" run-name "/" (db:test-make-full-name test-name item-path)))
	      ;; note the trailing slash to get the dir inspite of it being a link
	      (test-path         (conc linktree "/" test-partial-path))
	      (test-physical-path (if (file-exists? test-path) (read-symbolic-link test-path #t) #f))
	      (partial-path-index (if test-physical-path (substring-index test-partial-path test-physical-path) #f))
	      (test-base         (if (and partial-path-index 
					  test-physical-path )
				     (substring test-physical-path
						0
						partial-path-index)
				     #f)))
	 
 	 (if (or toplevel/children
		 (not (file-exists? test-path)))
	     #f
	     (begin
	       (debug:print 0
			    "From test-dat=" test-dat " derived the following:\n"
			    "test-partial-path  = " test-partial-path "\n"
			    "test-path          = " test-path "\n"
			    "test-physical-path = " test-physical-path "\n"
			    "partial-path-index = " partial-path-index "\n"
			    "test-base          = " test-base)
	       (hash-table-set! disk-groups test-base (cons test-physical-path (hash-table-ref/default disk-groups test-base '())))
	       (hash-table-set! test-groups test-base (cons test-dat (hash-table-ref/default test-groups test-base '())))
	       test-path))))
     tests)
    ;; for each disk-group
    (for-each 
     (lambda (disk-group)
       (debug:print 0 "Processing disk-group " disk-group)
       (let* ((test-paths (hash-table-ref disk-groups disk-group))
	      ;; ((string-intersperse (map cadr (rmt:get-key-val-pairs 1)) "-")
	      (bup-init-params  (list "-d" archive-dir "init"))
	      (bup-index-params (append (list "-d" archive-dir "index") test-paths))
	      (bup-save-params  (append (list "-d" archive-dir "save" ;; (conc "--strip-path=" linktree)
					      (conc "-" compress) ;; or (conc "--compress=" compress)
					      "-n" (conc (common:get-testsuite-name) "-" run-id)
					      (conc "--strip-path=" disk-group))
					test-paths))
	      (print-prefix      #f)) ;; "Running: ")) ;; change to #f to turn off printing
	 (if (not (file-exists? archive-dir))
	     (create-directory archive-dir #t))
	 (if (not (file-exists? (conc archive-dir "/HEAD")))
	     (begin
	       ;; replace this with jobrunner stuff enventually
	       (debug:print-info 0 "Init bup in " archive-dir)
	       (run-n-wait bup-exe params: bup-init-params print-cmd: print-prefix)))
	 (debug:print-info 0 "Indexing data to be archived")
	 (run-n-wait bup-exe params: bup-index-params print-cmd: print-prefix)
	 (debug:print-info 0 "Archiving data with bup")
	 (run-n-wait bup-exe params: bup-save-params print-cmd: print-prefix)
	 (for-each
	  (lambda (test-dat)
	    (let ((test-id           (db:test-get-id        test-dat))
		  (run-id            (db:test-get-run_id    test-dat)))
	      (rmt:test-set-archive-block-id run-id test-id archive-id)
	      (if (member archive-command '("save-remove"))
		  (runs:remove-test-directory test-dat 'archive-remove))))
	  (hash-table-ref test-groups disk-group))))
     (hash-table-keys disk-groups))
    #t))

(define (archive:bup-restore archive-command run-id run-name tests area-dat)  ;; move the getting of archive space down into the below block so that a single run can 
  ;; allocate as needed should a disk fill up
  ;;
  (let* ((configdat    (megatest:area-configdat area-dat))
	 (bup-exe      (or (configf:lookup configdat "archive" "bup") "bup"))
	 (linktree     (configf:lookup configdat "setup" "linktree")))

    ;; from the test info bin the path to the test by stem
    ;;
    (for-each
     (lambda (test-dat)
       ;; When restoring test-dat will initially contain an old and invalid path to the test
       (let* ((best-disk         (get-best-disk configdat))
	      (item-path         (db:test-get-item-path test-dat))
	      (test-name         (db:test-get-testname  test-dat))
	      (test-id           (db:test-get-id        test-dat))
	      (run-id            (db:test-get-run_id    test-dat))
	      (keyvals           (rmt:get-key-val-pairs run-id))
	      (target            (string-intersperse (map cadr keyvals) "/"))
	      
	      (toplevel/children (and (db:test-get-is-toplevel test-dat)
				      (> (rmt:test-toplevel-num-items run-id test-name) 0)))
	      (test-partial-path (conc target "/" run-name "/" (db:test-make-full-name test-name item-path)))
	      ;; note the trailing slash to get the dir inspite of it being a link
	      (test-path         (conc linktree "/" test-partial-path))
	      ;; if the old path was not deleted then prev-test-physical-path will end up pointing to a real directory
	      (prev-test-physical-path (if (file-exists? test-path) (read-symbolic-link test-path #t) #f))

	      (new-test-physical-path  (conc best-disk "/" test-partial-path))
	      (archive-block-id        (db:test-get-archived test-dat))
	      (archive-block-info      (rmt:test-get-archive-block-info archive-block-id))
	      (archive-path            (if (vector? archive-block-info)
					   (vector-ref archive-block-info 2) ;; look in db.scm for test-get-archive-block-info for the vector record info
					   #f)) ;; no archive found?
	      (archive-internal-path   (conc (common:get-testsuite-name) "-" run-id "/latest/" test-partial-path)))
	 
	 ;; some sanity checks 
	 (if (and prev-test-physical-path
		  (file-exists? prev-test-physical-path)) ;; what to do? abort or clean up or link it in?
	     (debug:print 0 "ERROR: the old directory " prev-test-physical-path ", still exists! This should not be."))

	 (if archive-path ;; no point in proceeding if there is no actual archive
	     (begin
	       ;; CREATE WORK AREA
	       ;; test-src-path == #f     ==> don't copy in data from tests directory
	       ;; itemdat       == string ==> use directly
	       (create-work-area run-id run-name keyvals test-id #f best-disk test-name item-path) ;; #!key (remtries 2))

	       ;; 1. Get the block id from the test info
	       ;; 2. Get the block data given the block id
	       ;; 3. Construct the paths etc. for the following command:
	       ;; 
	       ;; bup -d /tmp/matt/adisk1/2015_q1/fullrun_e1a40/ restore -C /tmp/seeme fullrun-30/latest/ubuntu/nfs/none/w02.1.20.54_b/

	       ;; DO BUP RESTORE
	       (let* ((new-test-dat        (rmt:get-test-info-by-id run-id test-id))
		      (new-test-path       (if (vector? new-test-dat )
					       (db:test-get-rundir new-test-dat)
					       (begin
						 (debug:print 0 "ERROR: unable to get data for run-id=" run-id ", test-id=" test-id)
						 (exit 1))))
		      ;; new-test-path won't work - must use best-disk instead? Nope, new-test-path but tack on /..
		      (bup-restore-params  (list "-d" archive-path "restore" "-C" (conc new-test-path "/..") archive-internal-path)))
		 (debug:print-info 0 "Restoring archived data to " new-test-physical-path " from archive in " archive-path " ... " archive-internal-path)
		 (run-n-wait bup-exe params: bup-restore-params print-cmd: #f)))
	     (debug:print 0 "ERROR: No archive path in the record for run-id=" run-id " test-id=" test-id))))
     (filter vector? tests))))
	 