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
;; THESE ARE ALL CALLED ON THE CLIENT SIDE!!!
;;

;;======================================================================
;;  S U P P O R T   F U N C T I O N S
;;======================================================================

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

;;======================================================================
;;
;; A C T U A L   A P I   C A L L S  
;;
;;======================================================================

;;======================================================================
;;  A D M I N
;;======================================================================

(define (rmt:login)
  (rmt:send-receive 'login (list *toppath* megatest-version *my-client-signature*)))

;;======================================================================
;;  K E Y S 
;;======================================================================

(define (rmt:get-key-val-pairs run-id)
  (rmt:send-receive 'get-key-val-pairs (list run-id)))

;;======================================================================
;;  T E S T S
;;======================================================================

(define (rmt:get-test-info-by-id test-id)
  (let ((res (rmt:send-receive 'get-test-info-by-id (list test-id))))
    (if (list? res)
	(list->vector res)
	res)))

(define (rmt:test-get-rundir-from-test-id test-id)
  (rmt:send-receive 'test-get-rundir-from-test-id (list test-id)))

(define (rmt:open-test-db-by-test-id test-id #!key (work-area #f))
  (let* ((test-path (if (string? work-area)
			work-area
			(rmt:test-get-rundir-from-test-id test-id))))
    (debug:print 3 "TEST PATH: " test-path)
    (open-test-db test-path)))

(define (rmt:testmeta-get-record testname)
  (list->vector
   (rmt:send-receive 'testmeta-get-record (list testname))))

;; WARNING: This currently bypasses the transaction wrapped writes system
(define (rmt:test-set-state-status-by-id test-id newstate newstatus newcomment)
  (rmt:send-receive 'test-set-state-status-by-id (list test-id newstate newstatus newcomment)))

;;======================================================================
;;  R U N S
;;======================================================================

(define (rmt:get-run-info run-id)
  (let ((res (rmt:send-receive 'get-run-info (list run-id))))
    (vector (car res)
	    (list->vector (cadr res)))))

(define (rmt:register-run keyvals runname state status user)
  (rmt:send-receive 'register-run (list keyvals runname state status user)))
    

;;======================================================================
;;  S T E P S
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

;;======================================================================
;;  T E S T   D A T A 
;;======================================================================

(define (rmt:read-test-data test-id categorypatt #!key (work-area #f)) 
  (let ((tdb  (rmt:open-test-db-by-test-id test-id work-area: work-area)))
    (if tdb
	(tdb:read-test-data tdb test-id categorypatt)
	'())))
