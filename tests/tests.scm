;; Copyright 2006-2012, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

;;  strftime('%m/%d/%Y %H:%M:%S','now','localtime')

(require-extension test)
(require-extension regex)
(require-extension srfi-18)
(import srfi-18)
;; (require-extension zmq)
;; (import zmq)

(define test-work-dir (current-directory))

;; read in all the _record files
(let ((files (glob "*_records.scm")))
  (for-each
   (lambda (file)
     (print "Loading " file)
     (load file))
   files))

(define *runremote* #f)

(let* ((unit-test-name (list-ref (argv) 4))
       (fname          (conc "../unittests/" unit-test-name ".scm")))
  (if (file-exists? fname)
      (load fname)
      (print "ERROR: Unit test " unit-test-name " not found in unittests directory")))

