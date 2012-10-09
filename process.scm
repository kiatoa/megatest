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

(declare (unit process))
(declare (uses common))

(define (conservative-read port)
  (let loop ((res ""))
    (if (not (eof-object? (peek-char port)))
	(loop (conc res (read-char port)))
	res)))
    
(define (cmd-run-with-stderr->list cmd . params)
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
	 (let ((errstr (conservative-read fhe)))
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
	     (close-input-port fhe)
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
  