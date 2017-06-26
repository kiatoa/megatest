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

;;======================================================================
;; Process convience utils
;;======================================================================

(use regex)
(declare (unit process))

(define (process:conservative-read port)
  (let loop ((res ""))
    (if (not (eof-object? (peek-char port)))
	(loop (conc res (read-char port)))
	res)))

(define (process:cmd-run-with-stderr->list cmd . params)
  ;; (print "Called with cmd=" cmd ", proc=" proc ", params=" params)
;;  (handle-exceptions
;;   exn
;;   (begin
;;     (print "ERROR:  Failed to run command: " cmd " " (string-intersperse params " "))
;;     (print "       " ((condition-property-accessor 'exn 'message) exn))
;;     #f)
   (let-values (((fh fho pid fhe) (if (null? params)
				      (process* cmd)
				      (process* cmd params))))
       (let loop ((curr (read-line fh))
		  (result  '()))
	 (let ((errstr (process:conservative-read fhe)))
	   (if (not (string=? errstr ""))
	       (set! result (append result (list errstr)))))
       (if (not (eof-object? curr))
	   (loop (read-line fh)
		 (append result (list curr)))
	   (begin
	     (close-input-port fh)
	     (close-input-port fhe)
	     (close-output-port fho)
	     result))))) ;; )

(define (process:cmd-run-proc-each-line cmd proc . params)
  ;; (print "Called with cmd=" cmd ", proc=" proc ", params=" params)
  (handle-exceptions
   exn
   (begin
     (print "ERROR:  Failed to run command: " cmd " " (string-intersperse params " "))
     (debug:print 0 *default-log-port* " message: " ((condition-property-accessor 'exn 'message) exn))
     (debug:print 5 *default-log-port* "exn=" (condition->list exn))
     #f)
   (let-values (((fh fho pid) (if (null? params)
				  (process cmd)
				  (process cmd params))))
       (let loop ((curr (read-line fh))
		(result  '()))
       (if (not (eof-object? curr))
	   (loop (read-line fh)
		 (append result (list (proc curr))))
	   (begin
	     (close-input-port fh)
	     (close-input-port fhe)
	     (close-output-port fho)
	     result))))))

(define (process:cmd-run-proc-each-line-alt cmd proc)
  (let* ((fh (open-input-pipe cmd))
         (res (port-proc->list fh proc))
         (status (close-input-pipe fh)))
    (if (eq? status 0) res #f)))

(define (process:cmd-run->list cmd)
  (let* ((fh (open-input-pipe cmd))
         (res (port->list fh))
         (status (close-input-pipe fh)))
    (list res status)))

(define (port->list fh)
  (if (eof-object? fh) #f
      (let loop ((curr (read-line fh))
                 (result '()))
        (if (not (eof-object? curr))
            (loop (read-line fh)
                  (append result (list curr)))
            result))))

(define (port-proc->list fh proc)
  (if (eof-object? fh) #f
      (let loop ((curr (proc (read-line fh)))
                 (result '()))
        (if (not (eof-object? curr))
            (loop (let ((l (read-line fh)))
                    (if (eof-object? l) l (proc l)))
                  (append result (list curr)))
            result))))

;; here is an example line where the shell is sh or bash
;; "find / -print 2&>1 > findall.log"
(define (run-n-wait cmdline #!key (params #f)(print-cmd #f))
  (if print-cmd 
      (debug:print 0 *default-log-port* 
		   (if (string? print-cmd)
		       print-cmd
		       "")
		   cmdline
		   (if params
		       (string-intersperse params " ")
		       "")))
  (let ((pid (if params
		 (process-run cmdline params)
		 (process-run cmdline))))
    (let loop ((i 0))
      (let-values (((pid-val exit-status exit-code) (process-wait pid #t)))
         (if (eq? pid-val 0)
	     (begin
	       (thread-sleep! 2)
	       (loop (+ i 1)))
	     (values pid-val exit-status exit-code))))))
  
;;======================================================================
;; MISC PROCESS RELATED STUFF
;;======================================================================

(define (process:children proc)
  (with-input-from-pipe
   (conc "ps h --ppid " (current-process-id) " -o pid")
   (lambda ()
     (let loop ((inl (read-line))
		(res '()))
       (if (eof-object? inl)
	   (reverse res)
	   (let ((pid (string->number inl)))
	     (if proc (proc pid))
	     (loop (read-line) (cons pid res))))))))

(define (process:alive? pid)
  (handle-exceptions
   exn
   ;; possibly pid is a process not a child, look in /proc to see if it is running still
   (common:file-exists? (conc "/proc/" pid))
   (let-values (((rpid exit-type exit-signal)(process-wait pid #t)))
       (and (number? rpid)
	    (equal? rpid pid)))))

(define (process:alive-on-host? host pid)
  (let ((cmd (conc "ssh " host " ps -o pid= -p " pid)))
    (handle-exceptions
     exn
     #f ;; anything goes wrong - assume the process in NOT running.
     (with-input-from-pipe 
      cmd
      (lambda ()
	(let loop ((inl (read-line)))
	  (if (eof-object? inl)
	      #f
	      (let* ((clean-str (string-substitute "^[^\\d]*([0-9]+)[^\\d]*$" "\\1" inl))
		     (innum     (string->number clean-str)))
		(and innum
		     (eq? pid innum))))))))))

(define (process:get-sub-pids pid)
  (with-input-from-pipe
   (conc "pstree -A -p " pid) ;; | tr 'a-z\\-+`()\\.' ' ' " pid)
   (lambda ()
     (let loop ((inl (read-line))
		(res '()))
       (if (eof-object? inl)
	   (reverse res)
	   (let ((nums (map string->number
			    (string-split-fields "\\d+" inl))))
	     (loop (read-line)
		   (append res nums))))))))
