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

(declare (unit api))
(declare (uses rmt))
(declare (uses db))

;; These are called by the server on recipt of /api calls

(define (api:execute-requests db cmd params)
  (case (string->symbol cmd)
    ;; KEYS
    ((get-key-val-pairs)            (apply db:get-key-val-pairs db params))
    ;; TESTS
    ;; json doesn't do vectors, convert to list
    ((get-test-info-by-id)	    (let ((res (apply db:get-test-info-by-id db params)))
				      (if (vector? res)(vector->list res) res)))
    ((test-get-rundir-from-test-id) (apply db:test-get-rundir-from-test-id db params))
    ((testmeta-get-record)          (vector->list (apply db:testmeta-get-record db params)))
    ((test-set-state-status-by-id)  (apply db:test-set-state-status-by-id db params))
    ;; RUNS
    ((get-run-info)                 (let ((res (apply db:get-run-info db params)))
				      (list (vector-ref res 0)
					    (vector->list (vector-ref res 1)))))
    ((register-run)                 (apply db:register-run db params))
    ((login)                        (apply db:login db params))
    (else
     (list "ERROR" 0))))

;; http-server  send-response
;;                 api:process-request
;;                    db:*
;;
;; NB// Runs on the server as part of the server loop
;;
(define (api:process-request db $) ;; the $ is the request vars proc
  (let* ((cmd     ($ 'cmd))
	 (paramsj ($ 'params))
	 (params  (rmt:json-str->dat paramsj))
	 (res     (api:execute-requests db cmd params)))
    (rmt:dat->json-str
     (if (or (string? res)
	     (list?   res)
	     (number? res)
	     (boolean? res))
	 res 
	 (list "ERROR" 1 cmd params res)))))

