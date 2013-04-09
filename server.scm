
;; Copyright 2006-2012, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

(require-extension (srfi 18) extras tcp s11n)

(use srfi-1 posix regex regex-case srfi-69 hostinfo md5 message-digest zmq)

(use spiffy uri-common intarweb http-client spiffy-request-vars)

(declare (unit server))

(declare (uses common))
(declare (uses db))
(declare (uses tasks)) ;; tasks are where stuff is maintained about what is running.
(declare (uses synchash))
(declare (uses http-transport))
(declare (uses zmq-transport))
(declare (uses daemon))

(include "common_records.scm")
(include "db_records.scm")

(define (server:make-server-url hostport)
  (if (not hostport)
      #f
      (conc "http://" (car hostport) ":" (cadr hostport))))

(define  *server-loop-heart-beat* (current-seconds))
(define *heartbeat-mutex* (make-mutex))

;;======================================================================
;; S E R V E R
;;======================================================================

;; Call this to start the actual server
;;

;; all routes though here end in exit ...
(define (server:launch transport)
  (if (not *toppath*)
      (if (not (setup-for-run))
	  (begin
	    (debug:print 0 "ERROR: cannot find megatest.config, exiting")
	    (exit))))
  (debug:print-info 2 "Starting server using " transport " transport")
  (set! *transport-type* transport)
  (case transport
    ((fs)   (exit)) ;; there is no "fs" transport
    ((http) (http-transport:launch))
    ((zmq)  (zmq-transport:launch))
    (else
     (debug:print "WARNING: unrecognised transport " transport)
     (exit))))

;;======================================================================
;; Q U E U E   M A N A G E M E N T
;;======================================================================

;; We don't want to flush the queue if it was just flushed
(define *server:last-write-flush* (current-milliseconds))

;; Flush the queue every third of a second. Can we assume that setup-for-run 
;; has already been done?
(define (server:write-queue-handler)
  (if (setup-for-run)
      (let ((db (open-db)))
	(let loop ()
	  (let ((last-write-flush-time #f))
	    (mutex-lock! *incoming-mutex*)
	    (set! last-write-flush-time *server:last-write-flush*)
	    (mutex-unlock! *incoming-mutex*)
	    (if (> (- (current-milliseconds) last-write-flush-time) 400)
		(begin
		  (mutex-lock! *db:process-queue-mutex*)
		  (db:process-cached-writes db)
		  (mutex-unlock! *db:process-queue-mutex*)
		  (thread-sleep! 0.5))))
	  (loop)))
      (begin
	(debug:print 0 "ERROR: failed to setup for Megatest in server:write-queue-handler")
	(exit 1))))
    
;;======================================================================
;; S E R V E R   U T I L I T I E S 
;;======================================================================

;; Generate a unique signature for this server
(define (server:mk-signature)
  (message-digest-string (md5-primitive) 
			 (with-output-to-string
			   (lambda ()
			     (write (list (current-directory)
					  (argv)))))))


;; When using zmq this would send the message back (two step process)
;; with spiffy or rpc this simply returns the return data to be returned
;; 
(define (server:reply return-addr query-sig success/fail result)
  (debug:print-info 11 "server:reply return-addr=" return-addr ", result=" result)
  ;; (send-message pubsock target send-more: #t)
  ;; (send-message pubsock 
  (case *transport-type*
    ((fs) result)
    ((http)(db:obj->string (vector success/fail query-sig result)))
    ((zmq)
     (let ((pub-socket (vector-ref *runremote* 1)))
       (send-message pub-socket return-addr send-more: #t)
       (send-message pub-socket (db:obj->string (vector success/fail query-sig result)))))
    (else 
     (debug:print 0 "ERROR: unrecognised transport type: " *transport-type*)
     result)))

