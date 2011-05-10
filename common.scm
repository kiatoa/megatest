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

(use sqlite3 srfi-1 posix regex-case base64 format)
(require-extension sqlite3 regex posix)

(import (prefix sqlite3 sqlite3:))
(import (prefix base64 base64:))

;; (require-library margs)
(include "margs.scm")

(define getenv get-environment-variable)

(define home (getenv "HOME"))
(define user (getenv "USER"))

(define *configinfo* #f)
(define *configdat*  #f)
(define *toppath*    #f)
(define *already-seen-runconfig-info* #f)
(define *waiting-queue* (make-hash-table))
(define *globalexitstatus* 0) ;; attempt to work around possible thread issues

(define-inline (get-with-default val default)
  (let ((val (args:get-arg val)))
    (if val val default)))

(define-inline (assoc/default key lst . default)
  (let ((res (assoc key lst)))
    (if res (cadr res)(if (null? default) #f (car default)))))

;;======================================================================
;; Misc utils
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
        (whitesp (regexp "[^a-zA-Z0-9_\\-:;,.\\/%]")))
     (with-output-to-file (conc fname ".csh")
       (lambda ()
          (for-each (lambda (key)
                      (let* ((val (cdr key))
                             (sval (if (string-search whitesp val)(conc "'" val "'") val)))
                        (print "setenv " (car key) " " sval)))
                     envvars)))
     (with-output-to-file (conc fname ".sh")
       (lambda ()
          (for-each (lambda (key)
                      (let* ((val (cdr key))
                             (sval (if (string-search whitesp val)(conc "'" val "'") val)))
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
		  
