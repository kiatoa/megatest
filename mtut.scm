;; Copyright 2006-2017, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

;; (include "common.scm")
;; (include "megatest-version.scm")

;; fake out readline usage of toplevel-command
(define (toplevel-command . a) #f)

(use srfi-1 posix srfi-69 readline ;;  regex regex-case srfi-69 apropos json http-client directory-utils rpc typed-records;; (srfi 18) extras)
     srfi-18 extras format pkts) ;;  zmq extras)

(declare (uses common))
(declare (uses megatest-version))
(declare (uses margs))
;; (declare (uses runs))
;; (declare (uses launch))
;; (declare (uses server))
;; (declare (uses client))
;; (declare (uses tests))
;; (declare (uses genexample))
;; (declare (uses daemon))
;; (declare (uses db))
;; ;; (declare (uses dcommon))
;; 
;; (declare (uses tdb))
;; (declare (uses mt))
;; (declare (uses api))
;; (declare (uses tasks)) ;; only used for debugging.
;; (declare (uses env))
;; (declare (uses diff-report))
;; 
;; (define *db* #f) ;; this is only for the repl, do not use in general!!!!

;; (include "common_records.scm")
;; (include "key_records.scm")
;; (include "db_records.scm")
;; (include "run_records.scm")
(include "megatest-fossil-hash.scm")

(let ((debugcontrolf (conc (get-environment-variable "HOME") "/.mtutilrc")))
  (if (file-exists? debugcontrolf)
      (load debugcontrolf)))

;; Disabled help items
;;  -rollup                 : (currently disabled) fill run (set by :runname)  with latest test(s)
;;                            from prior runs with same keys

(define help (conc "
mtutil, part of the Megatest tool suite, documentation at http://www.kiatoa.com/fossils/megatest
  version " megatest-version "
  license GPL, Copyright Matt Welland 2006-2017

Usage: mtutil action [options]
  -h                       : this help
  -manual                  : show the Megatest user manual
  -version                 : print megatest version (currently " megatest-version ")

Actions include:
   run                     : initial runs
   remove                  : remove runs
   rerun                   : register action for processing
   set-ss                  : set state/status
   archive                 : compress and move test data to archive disk
   kill                    : stop tests or entire runs

Selectors 
  -action-mode immediate|queued : apply this action after other actions or immediately
  -area areapatt1,area2... : apply this action only to the specified areas
  -target key1/key2/...    : run for key1, key2, etc.
  -test-patt p1/p2,p3/...  : % is wildcard
  -run-name                : required, name for this particular test run
  -state-status c/p,c/f    : Specify a list of state and status patterns
  -mode-patt key           : load testpatt from <key> in runconfigs instead of default TESTPATT
                             if -testpatt and -tagexpr are not specified
  -tag-expr tag1,tag2%,..  : select tests with tags matching expression
  -new state/status        : specify new state/status for set-ss

Misc 
  -start-dir path          : switch to this directory before running mtutil
  -set-vars V1=1,V2=2      : Add environment variables to a run NB// these are
                                 overwritten by values set in config files.
  -log logfile             : send stdout and stderr to logfile
  -repl                    : start a repl (useful for extending megatest)
  -load file.scm           : load and run file.scm
  -debug N|N,M,O...        : enable debug messages 0-N or N and M and O ...

Examples:

# Start a megatest run in the area \"mytests\"
mtutil -area mytests -action run -target v1.63/aa3e -modepatt MYPATT -tagexpr quick

Called as " (string-intersperse (argv) " ") "
Version " megatest-version ", built from " megatest-fossil-hash ))

;; args and pkt key specs
;;
(define *arg-keys*
  '(("-run"        . r)
    ("-target"     . t)
    ("-run-name"   . n)
    ("-state"      . e)
    ("-status"     . u)
    ("-test-patt"  . p)  ;; idea, enhance margs ("-test-patt" "-testpatt") => yields one value in "-test-patt"
    ("-mode-patt"  . o)
    ("-tag-expr"   . x)
    ("-item-patt"  . i)
    ;; misc
    ("-start-dir"  . #f)
    ("-set-vars"   . v)
    ("-debug"      . #f)  ;; for *verbosity* > 2
    ("-load"       . #f)  ;; load and exectute a scheme file
    ("-log"        . #f)
    ))
(define *switch-keys*
  '(("-h"          . #f)
    ("-help"       . #f)
    ("--help"      . #f)
    ("-manual"     . #f)
    ("-version"    . #f)
    ;; misc
    ("-repl"       . #f)))

;; process args
(define *action* (if (> (length (argv)) 1)
		     (cadr (argv))
		     #f))
(define remargs (args:get-args 
		 (if *action* (cdr (argv)) (argv)) ;; args:get-args dumps first in argv list (the program name)
		 (map car *arg-keys*)
		 (map car *switch-keys*)
		 args:arg-hash
		 0))

(print "*action*: " *action*)

;; Add args that use remargs here
;;
(if (and (not (null? remargs))
	 (not (or
	       (args:get-arg "-runstep")
	       (args:get-arg "-envcap")
	       (args:get-arg "-envdelta")
	       )
	      ))
    (debug:print-error 0 *default-log-port* "Unrecognised arguments: " (string-intersperse (if (list? remargs) remargs (argv))  " ")))

;;======================================================================
;; Runs
;;======================================================================

;; collect, translate, collate and assemble a pkt from the command-line
;;
(define (command-line->pkt args args-hash)
  (let* ((args-data (hash-table->alist args:arg-hash))
	 (alldat    (apply append (list 'a *action*)
			   (map (lambda (x)
				  (list (car x)(cdr x)))
				(filter cdr args-data)))))
    (print  "Alldat: " alldat
	    " args-data: " args-data)
    (add-z-card
     (apply construct-sdat alldat))))

(let-values (((uuid pkt)
	      (command-line->pkt #f args:arg-hash)))
  (print pkt))

(if (or (args:get-arg "-repl")
	(args:get-arg "-load"))
    (begin
      (import extras) ;; might not be needed
      ;; (import csi)
      (import readline)
      (import apropos)
      ;; (import (prefix sqlite3 sqlite3:)) ;; doesn't work ...
      
      (install-history-file (get-environment-variable "HOME") ".mtutil_history") ;;  [homedir] [filename] [nlines])
      (current-input-port (make-readline-port "mtutil> "))
      (if (args:get-arg "-repl")
	  (repl)
	  (load (args:get-arg "-load")))))