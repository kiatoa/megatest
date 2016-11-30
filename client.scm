
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

(use srfi-1 posix regex regex-case srfi-69 hostinfo md5 message-digest)
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

;; BB: commenting out orphan code
;;;;;
;; (define (client:connect iface port)
;;   (case (server:get-transport)
;;     ((rpc)  (rpc:client-connect  iface port))
;;     ((http) (http:client-connect iface port))
;;     ((zmq)  (zmq:client-connect  iface port))
;;     (else   (rpc:client-connect  iface port))))

(define (client:setup run-id #!key (remaining-tries 10))
  ;;(BB> "Entered client:setup with run-id="run-id" and remaining-tries="remaining-tries)
   
  (debug:print-info 2 *default-log-port* "client:setup remaining-tries=" remaining-tries)
  (let* ((server-dat (tasks:bb-get-server-info run-id))
         (transport (if (and server-dat (vector? server-dat)) (string->symbol (tasks:hostinfo-get-transport server-dat)) 'noserver)))
    
    (case transport
      ((noserver) ;; no server registered
       (BB> "noserver")
       (if (<= remaining-tries 0)
           (begin
             (debug:print-error 0 *default-log-port* "failed to start or connect to server for run-id " run-id)
             (exit 1))
           (begin    
             (let ((num-available (tasks:bb-num-in-available-state run-id)))
               (debug:print-info 0 *default-log-port* "client:setup, no server registered, remaining-tries=" remaining-tries " num-available=" num-available)
               (if (< num-available 2)
                   (server:try-running run-id))
               (thread-sleep! (+ 2 (random (- 20 remaining-tries))))  ;; give server a little time to start up, randomize a little to avoid start storms.
               (client:setup run-id remaining-tries: (- remaining-tries 1))))))
      ((http)  (client:setup-http run-id server-dat remaining-tries))
      ((rpc)  (rpc-transport:client-setup run-id server-dat remtries: remaining-tries)) 
      (else
       (debug:print-error 0 *default-log-port* "(6) Transport ["
                          transport "] specified for run-id [" run-id "] is not implemented in client:setup.  Cannot proceed.")
       (exit 1)))))


;; client:setup-http
;;
;; For http transport, robustly ensure an advertised-running server is actually working and responding, and
;; establish tcp connection to server.  For servers marked running but not responding, kill them and clear from mdb
;; 
(define (client:setup-http run-id server-dat remaining-tries)
  (let* ((iface     (tasks:hostinfo-get-interface server-dat))
         (hostname  (tasks:hostinfo-get-hostname  server-dat))
         (port      (tasks:hostinfo-get-port      server-dat))

         (start-res (http-transport:client-connect iface port))
         (ping-res  (rmt:login-no-auto-client-setup start-res run-id)))
    (if (and start-res ping-res)
        (begin
          (rmt:set-cinfo run-id start-res) ;; (hash-table-set! *runremote* run-id start-res) ;; side-effect - *runremote* cache init fpr rmt:*
          (debug:print-info 2 *default-log-port* "connected to " (http-transport:server-dat-make-url start-res))
          start-res)
        (begin    ;; login failed but have a server record, clean out the record and try again
          (debug:print-info 0 *default-log-port* "client:setup-http, login failed, will attempt to start server ... start-res=" start-res ", run-id=" run-id ", server-dat=" server-dat)
          (http-transport:close-connections run-id)
          (rmt:del-cinfo run-id) ;; (hash-table-delete! *runremote* run-id) ;; BB: suspect there is nothing to delete ...
          (tasks:kill-server-run-id run-id) ;; -9 so the hung processes dont eat 100% when not responding to sigterm.
          (tasks:bb-server-force-clean-run-record  run-id iface port
                                                   " client:setup-http (server-dat = #t)")
          (if (> remaining-tries 8)
              (thread-sleep! (+ 1 (random 5))) ;; spread out the starts a little
              (thread-sleep! (+ 15 (random 20)))) ;; it isn't going well. give it plenty of time
          (server:try-running run-id)
          (thread-sleep! 5)   ;; give server a little time to start up
          (client:setup run-id remaining-tries: (- remaining-tries 1))
          ))))

