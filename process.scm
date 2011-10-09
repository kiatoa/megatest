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

;;======================================================================
;; Process convience utils
;;======================================================================

(declare (unit process))
(declare (uses common))

(define (cmd-run-proc-each-line cmd proc . params)
  ;; (print "Called with cmd=" cmd ", proc=" proc ", params=" params)
  (handle-exceptions
   exn
   (begin
     (print "ERROR:  Failed to run command: " cmd " " (string-intersperse params " "))
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
	     (close-output-port fho)
	     result))))))

(define (cmd-run-proc-each-line-alt cmd proc)
  (let* ((fh (open-input-pipe cmd))
         (res (port-proc->list fh proc))
         (status (close-input-pipe fh)))
    (if (eq? status 0) res #f)))

(define (cmd-run->list cmd)
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
(define (run-n-wait cmdline)
  (let ((pid (process-run cmdline)))
    (let loop ((i 0))
      (let-values (((pid-val exit-status exit-code) (process-wait pid #t)))
         (if (eq? pid-val 0)
	     (begin
	       (thread-sleep! 2)
	       (loop (+ i 1)))
	     (values pid-val exit-status exit-code))))))
  