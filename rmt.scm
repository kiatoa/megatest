;;======================================================================
;; Copyright 2006-2013, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.
;;======================================================================

(use json)

(declare (unit rmt))
(declare (uses api))
(declare (uses tdb))
(declare (uses http-transport))

;;
;; These are all called on the client side
;;

;; cmd is a symbol
;; vars is a json string encoding the parameters for the call
;;
(define (rmt:send-receive cmd params)
  (case *transport-type* 
    ((fs)
     (debug:print 0 "ERROR: Not yet (re)supported")
     (exit 1))
    ((http)
     (let* ((jparams (rmt:dat->json-str params))
	    (res (http-transport:client-api-send-receive *runremote* cmd jparams)))
       (if res
	   (rmt:json-str->dat res)
	   (begin
	     (debug:print 0 "ERROR: Bad value from http-transport:client-api-send-receive " res)
	     #f))
     ))
    (else
     (debug:print 0 "ERROR: Transport not yet (re)supported")
     (exit 1))))

;; Wrap json library for strings (why the ports crap in the first place?)
(define (rmt:dat->json-str dat)
  (with-output-to-string 
    (lambda ()
      (json-write dat))))

(define (rmt:json-str->dat json-str)
  (with-input-from-string json-str
    (lambda ()
      (json-read))))

;;
;; Actual api calls 
;;

(define (rmt:get-test-info-by-id test-id)
  (list->vector
   (rmt:send-receive 'get-test-info-by-id (list test-id))))

(define (rmt:get-key-val-pairs run-id)
  (rmt:send-receive 'get-key-val-pairs (list run-id)))

(define (rmt:test-get-rundir-from-test-id test-id)
  (rmt:send-receive 'test-get-rundir-from-test-id (list test-id)))

(define (rmt:open-test-db-by-test-id test-id #!key (work-area #f))
  (let* ((test-path (if (string? work-area)
			work-area
			(rmt:test-get-rundir-from-test-id test-id))))
    (debug:print 3 "TEST PATH: " test-path)
    (open-test-db test-path)))

;;======================================================================
;; S T E P S
;;======================================================================

;; Getting steps is more complicated.
;;
;; If given work area 
;;  1. Find the testdat.db file
;;  2. Open the testdat.db file and do the query
;; If not given the work area
;;  1. Do a remote call to get the test path
;;  2. Continue as above
;; 
(define (rmt:get-steps-for-test test-id #!key (work-area #f))
  (let* ((tdb (rmt:open-test-db-by-test-id test-id work-area: work-area)))
    (if tdb
	(tdb:get-steps-data tdb test-id)
	'())))
