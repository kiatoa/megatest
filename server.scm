
;; Copyright 2006-2011, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

;; procstr is the name of the procedure to be called as a string
(define (server:autoremote procstr params)
  (handle-exceptions
   exn
   (begin
     (debug:print 1 "Remote failed for " proc " " params)
     (apply (eval (string->symbol proc)) params))
   (if *runremote*
       (apply (eval (string->symbol (conc "remote:" procstr))) params)
       (eval (string->symbol procstr) params))))

(define (server:start db)
  (debug:print 0 "Attempting to start the server ...")
  (let* ((rpc:listener (server:find-free-port-and-open (rpc:default-server-port)))
	 (th1          (make-thread
			(cute (rpc:make-server rpc:listener) "rpc:server")
			'rpc:server)))
    (db:set-var db "SERVER" (conc (get-host-name) ":" (rpc:default-server-port)))
    (rpc:publish-procedure! 
     'remote:run 
     (lambda (procstr . params)
       (server:autoremote procstr params)))
    (set! *rpc:listener* rpc:listener*)
    (thread-start! rpc:server)))

(define (server:find-free-port-and-open port)
  (handle-exceptions
   exn
   (begin
     (print "Failed to bind to port " (rpc:default-server-port) ", trying next port")
     (server:find-free-port-and-open (+ port 1)))
   (rpc:default-server-port port)
   (tcp-listen (rpc:default-server-port))))

(define (server:client-setup db)
  (let* ((hostinfo (db:get-var db "SERVER"))
	 (hostdat  (if hostinfo (string-split hostinfo ":")))
	 (host     (if hostinfo (car hostdat)))
	 (port     (if (and hostinfo (> (length hostdat) 1))(cadr hostdat) #f)))
    (set! *runremote* (vector host port))))
