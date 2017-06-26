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
(require-extension posix)
(import posix)
(import srfi-18)
;; (require-extension zmq)
;; (import zmq)

(define test-work-dir (current-directory))

;; given list of lists
;;  ( ( msg expected param1 param2 ...)
;;    ( ... ) )
;; apply test to all
;;
(define (test-batch proc pname inlst #!key (post-proc #f))
  (for-each
   (lambda (spec)
     (let ((msg    (conc pname " " (car spec)))
           (result (cadr spec))
           (params (cddr spec)))
       (if post-proc
           (test msg result (post-proc (apply proc params)))
           (test msg result (apply proc params)))))
   inlst))

;; read in all the _record files
(let ((files (glob "*_records.scm")))
  (for-each
   (lambda (file)
     (print "Loading " file)
     (load file))
   files))

(let* ((unit-test-name (list-ref (argv) 4))
       (fname          (conc "../unittests/" unit-test-name ".scm")))
  (if (file-exists? fname)
      (load fname)
      (print "ERROR: Unit test " unit-test-name " not found in unittests directory")))

;;; huh? why is this here?
;; (list "abc" "abc/%" "ab%/c%" "~abc/c%" "abc/~c%" "a,b/c,%/d" "%/,%/a" "%/,%/a" "%/,%/a" "%" "%" "%/" "%/" "%abc%")
;; (list "abc" "abc"   "abcd"   "abc"     "abc"     "a"         "abc"     "def"    "ghi"   "a" "a"  "a"  "a" "abc")
;; (list   ""  ""      "cde"    "cde"     "cde"     ""            ""      "a"       "b"    ""  "b"  ""   "b" "abc")
;; (list   #t    #t       #t    #f           #f      #t           #t       #t       #f     #t  #t   #t    #f #t)

