
;; Copyright 2006-2012, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

;;======================================================================
;; C L I E N T S
;;======================================================================

(require-extension (srfi 18) extras tcp s11n)

(use srfi-1 posix regex regex-case srfi-69 hostinfo md5 message-digest matchable)
;; (use zmq)

(use (prefix sqlite3 sqlite3:))

(use spiffy uri-common intarweb http-client spiffy-request-vars uri-common intarweb directory-utils)

(declare (unit client))

(declare (uses common))
(declare (uses db))
(declare (uses tasks)) ;; tasks are where stuff is maintained about what is running.

(include "common_records.scm")
(include "db_records.scm")

;; client:get-signature
(define (client:get-signature)
  (if *my-client-signature* *my-client-signature*
      (let ((sig (conc (get-host-name) " " (current-process-id))))
	(set! *my-client-signature* sig)
	*my-client-signature*)))

;; Not currently used! But, I think it *should* be used!!!
(define (client:logout serverdat)
  (let ((ok (and (socket? serverdat)
		 (cdb:logout serverdat *toppath* (client:get-signature)))))
    ok))

(define (client:connect iface port)
  (case (server:get-transport)
    ((rpc)  (rpc:client-connect  iface port))
    ((http) (http:client-connect iface port))
    ((zmq)  (zmq:client-connect  iface port))
    (else   (rpc:client-connect  iface port))))

(define (client:setup areapath #!key (remaining-tries 100) (failed-connects 0))
  (case (server:get-transport)
    ((rpc) (rpc-transport:client-setup remaining-tries: remaining-tries failed-connects: failed-connects)) ;;(client:setup-rpc run-id))
    ((http)(client:setup-http areapath remaining-tries: remaining-tries failed-connects: failed-connects))
    (else  (rpc-transport:client-setup remaining-tries: remaining-tries failed-connects: failed-connects)))) ;; (client:setup-rpc run-id))))

;; Do all the connection work, look up the transport type and set up the
;; connection if required.
;;
;; There are two scenarios. 
;;   1. We are a test manager and we received *transport-type* and *runremote* via cmdline
;;   2. We are a run tests, list runs or other interactive process and we must figure out
;;      *transport-type* and *runremote* from the monitor.db
;;
;; client:setup
;;
;; lookup_server, need to remove *runremote* stuff
;;

(define (client:setup-http areapath #!key (remaining-tries 100) (failed-connects 0)(area-dat #f))
  (debug:print-info 2 *default-log-port* "client:setup remaining-tries=" remaining-tries)
  (server:start-and-wait areapath)
  (if (<= remaining-tries 0)
      (begin
	(debug:print-error 0 *default-log-port* "failed to start or connect to server")
	(exit 1))
      ;;
      ;; Alternatively here, we can get the list of candidate servers and work our way
      ;; through them searching for a good one.
      ;;
      (let* ((server-dat (server:get-first-best areapath))
	     (runremote  (or area-dat *runremote*)))
	(if (not server-dat) ;; no server found
	    (client:setup-http areapath remaining-tries: (- remaining-tries 1))
	    (let ((host  (cadr  server-dat))
		  (port  (caddr server-dat)))
	      (debug:print-info 4 *default-log-port* "client:setup server-dat=" server-dat ", remaining-tries=" remaining-tries)
	      (if (and (not area-dat)
		       (not *runremote*))
		  (set! *runremote* (make-remote)))
	      (if (and host port)
		  (let* ((start-res (case *transport-type*
				      ((http)(http-transport:client-connect host port))))
			 (ping-res  (case *transport-type* 
				      ((http)(rmt:login-no-auto-client-setup start-res)))))
		    (if (and start-res
			     ping-res)
			(begin
			  (remote-conndat-set! runremote start-res) ;; (hash-table-set! runremote run-id start-res)
			  (debug:print-info 2 *default-log-port* "connected to " (http-transport:server-dat-make-url start-res))
			  start-res)
			(begin    ;; login failed but have a server record, clean out the record and try again
			  (debug:print-info 0 *default-log-port* "client:setup, login failed, will attempt to start server ... start-res=" start-res ", run-id=" run-id ", server-dat=" server-dat)
			  (case *transport-type* 
			    ((http)(http-transport:close-connections)))
			  (remote-conndat-set! runremote #f)  ;; (hash-table-delete! runremote run-id)
			  (thread-sleep! 1)
			  (client:setup-http areapath remaining-tries: (- remaining-tries 1))
			  )))
		  (begin    ;; no server registered
		    (server:kind-run areapath)
		    (debug:print-info 0 *default-log-port* "client:setup, no server registered, remaining-tries=" remaining-tries)
		    (thread-sleep! 1) ;; (+ 5 (random (- 20 remaining-tries))))  ;; give server a little time to start up, randomize a little to avoid start storms.
		    (server:start-and-wait areapath)
		    (client:setup-http areapath remaining-tries: (- remaining-tries 1)))))))))

