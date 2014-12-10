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
				  (safe-vector-ref block 1)   ;; archive-area-name
				  (safe-vector-ref block 2))) ;; disk-path
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

	       
