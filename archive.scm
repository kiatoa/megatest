;; Copyright 2006-2014, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

;;  strftime('%m/%d/%Y %H:%M:%S','now','localtime')

(use sqlite3 srfi-1 posix regex regex-case srfi-69 dot-locking format)
(import (prefix sqlite3 sqlite3:))

(declare (unit archive))
(declare (uses db))
(declare (uses common))

(include "common_records.scm")
(include "db_records.scm")

;;======================================================================
;; 
;;======================================================================

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
	   archive:run-bup
	   (list testdir apath))))))
	  
;; Get archive disks from megatest.config
;;
(define (archive:get-archive-disks)
  (let ((section (configf:get-section *configdat* "archivedisks")))
    (if section
	(map cdr section)
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
(define (archvie:allocate-new-archive-block testname itempath dneeded)
  (let* ((adisks    (archive:get-archive-disks))
	 (best-disk (common:get-disk-with-most-free-space adisks dneeded)))
    (if best-disk
	(let* ((bdisk-name    (car best-disk))
	       (bdisk-path    (cdr best-disk))
	       (bdisk-id      (rmt:archive-register-disk bdisk-name bdisk-path (get-df bdisk-path)))
	       (archive-name  (time->string (seconds->local-time (current-seconds)) "ww%W.%u"))
	       (archive-path  (conc bdisk-path "/" archive-name))
	       (block-id      (rmt:archive-register-block-name bdisk-id archive-path))
	       (allocation-id (rmt:archive-allocate-test-to-block block-id testname itempath)))
	  (if (and block-id allocation-id)
	      archive-path
	      #f)))))

;; archive - run bup
;;
;; 1. create the bup dir if not exists
;; 2. start the du of each directory
;; 3. gen index
;; 4. save
;;
(define (archive:run-bup archive-dir run-id run-name tests)
  (let* ((bup-exe    (or (configf:lookup *configdat* "archive" "bup") "bup"))
	 (compress   (or (configf:lookup *configdat* "archive" "compress") "9"))
	 (linktree   (configf:lookup *configdat* "setup" "linktree"))
	 (test-paths (filter
		      string?
		      (map (lambda (test-dat)
			     (let* ((item-path         (db:test-get-item-path test-dat))
				    (test-name         (db:test-get-testname  test-dat))
				    (run-id            (db:test-get-run_id    test-dat))
				    (target            (string-intersperse (map cadr (rmt:get-key-val-pairs run-id)) "/"))
				    
				    (toplevel/children (and (db:test-get-is-toplevel test-dat)
							    (> (rmt:test-toplevel-num-items run-id test-name) 0)))
				    ;; note the trailing slash to get the dir inspite of it being a link
				    (test-path         (conc linktree "/" target "/" run-name "/" (runs:make-full-test-name test-name item-path) "/")))
			       (if (or toplevel/children
				       (not (file-exists? test-path)))
				   #f
				   test-path)))
			   tests)))
	 ;; ((string-intersperse (map cadr (rmt:get-key-val-pairs 1)) "-")
	 (bup-init-params  (list "-d" archive-dir "init"))
	 (bup-index-params (append (list "-d" archive-dir "index") test-paths))
	 (bup-save-params  (append (list "-d" archive-dir "save" (conc "--strip-path=" linktree)
					 (conc "-" compress) ;; or (conc "--compress=" compress)
					  "-n" (conc (common:get-testsuite-name) "-" run-id))
				   test-paths))
	 (print-prefix     #f)) ;; "Running: ")) ;; change to #f to turn off printing
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
    #t))