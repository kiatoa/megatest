
(use nanomsg defstruct srfi-18)

;;======================================================================
;; Commands
;;======================================================================

(define *commands* (make-hash-table))

(defstruct cmd
  key
  proc
  ctype ;; command type; 'r (read), 'w (write) or 't (transaction)
  )

(define (register-command key ctype proc)
  (hash-table-set! *commands*
		   key
		   (make-cmd key: key ctype: ctype proc: proc)))

(define (get-proc key)
  (cmd-proc (hash-table-ref key *commands*)))

(for-each
 (lambda (dat)
   (apply register-command dat))
 `( (create-run    w ,create-run)
    (create-step   w ,create-step)
    (create-test   w ,create-test)
    (get-test-id   r ,get-test-id)
    (get-run-id    r ,get-run-id)
    ;; (open-db       w ,open-create-db)
    (step-set-ss   w ,step-set-state-status)
    (test-set-ss   w ,test-set-state-status)
    (test-get-tests r ,test-get-tests) ))

;;======================================================================
;; Server/client stuff
;;======================================================================

(define-inline (encode data)
  (with-output-to-string
    (lambda ()
      (write data))))

(define-inline (decode data)
  (with-input-from-string
      data
    (lambda ()
      (read))))
  
;;======================================================================
;; Command queue
;;======================================================================

(defstruct qitem
  command
  params
  host-port)

(define *cmd-queue* '())
(define *queue-mutex* (make-mutex))

(define (queue-push cmddat)
  (mutex-lock! *queue-mutex*)
  (set! *cmd-queue* (cons cmddat *cmd-queue*))
  (mutex-unlock! *queue-mutex*))

;; get all the cmds of type ctype and return them, also remove them from the queue
(define (queue-take ctype)
  (mutex-lock! *queue-mutex*)
  (let ((res (filter (lambda (x)(eq? (cmd-ctype x) ctype))       *cmd-queue*))
	(rem (filter (lambda (x)(not (eq? (cmd-ctype x) ctype))) *cmd-queue*)))
    (set! *queue* rem)
    (mutex-unlock! *queue-mutex*)
    res))

(define (queue-process-commands dbconn commands)
  (for-each
   (lambda (qitem)
     (let ((soc (request-connect (qitem-host-port qitem))) ;; we will be sending the data back to host-port via soc
	   (cmd (hash-table-ref/default *commands* (qitem-command qitem) #f)))
       (if cmd
	   (let* ((res (apply (get-proc cmd) dbconn (qitem-params qitem)))
		  (pkg (encode `((r . ,res)))))
	     (nn-send soc pkg)
	     (if (not (eq? (nn-recv soc)) "ok")
		 (print "Client failed to receive properly the data from " cmd " request"))))))
   commands))

;; the continuously running queue processor
;;
(define ((queue-processor dbconn))
  (let loop ()
    (queue-process-commands dbconn (queue-take 'r))  ;; reads first, probably largest numbers of them
    (queue-process-commands dbconn (queue-take 'w))  ;; writes next
    (queue-process-commands dbconn (queue-take 't))  ;; lastly process transactions
    (thread-sleep! 0.2)                              ;; open up the db for any other processes to access
    (loop)))

;;======================================================================
;; Client stuff
;;======================================================================

;; client struct
(defstruct client
  host-port
  socket
  last-access)

(define *clients* (make-hash-table)) ;; host:port -> client struct
(define *client-mutex* (make-mutex))

;; add a channel or return existing channel, this is a normal req
;; 
(define (request-connect host-port)
  (mutex-lock! *client-mutex*)
  (let* ((curr (hash-table-ref/default *clients* host-port #f)))
    (if curr
	(begin
	  (mutex-unlock! *client-mutex*)
	  curr)
	(let ((req (nn-socket 'req)))
	  (nn-connect req host-port) ;; "inproc://test")
	  (hash-table-set! *clients* host-port req)
	  (mutex-unlock! *client-mutex*)
	  req))))

;; open up a channel to the server and send a package of info for the server to act on
;; host-port needs to be found and provided
;;
(define (generic-db-access host-port)
  (let* ((soc (request-connect host-port))
	 ;; NEED *MY* host/port also to let the server know where to send the results
	 )))
    

(define (client-send-receive soc msg)
  (nn-send soc msg)
  (nn-recv soc))
  
;;======================================================================
;; Server
;;======================================================================

(defstruct srvdat
  host
  port
  soc)

;; remember, everyone starts a server, both client and the actual server alike.
;; clients start a server for the server to return results to.
;;
(define (start-raw-server #!key (given-host-name #f))
  (let ((srvdat    (let loop ((portnum 10000))
		     (handle-exceptions
			 exn
			 (if (< portnum 64000)
			     (loop (+ portnum 1))
			     #f)
		       (let* ((rep (nn-socket 'rep)))
			 (nn-bind rep (conc "tcp://*:" portnum)) ;; "inproc://test")
			 (make-srvdat port: portnum soc: rep)))))
	(host-name (or give-host-name (get-host-name)))
	(soc       (srvdat-soc srvdat)))
    (srvdat-host-set! srvdat host-name)
    srvdat))

;; The actual *server* side server
;;
(define (start-server dbconn #!key (given-host-name #f))
  (let* ((srvdat    (start-raw-server given-host-name: given-host-name))
	 (host-name (srvdat-host srvdat))
	 (soc       (srvdat-soc srvdat)))
    
    ;; start the queue processor
    (thread-start! (queue-processory dbconn) "Queue processor")
    ;; msg is an alist
    ;;  'r host:port  <== where to return the data
    ;;  'p params     <== data to apply the command to
    ;;  'e j|s|l      <== encoding of the params. default is s (sexp), if not specified is assumed to be default
    ;;  'c command    <== look up the function to call using this key
    ;;
    (let loop ((msg-in (nn-recv soc)))
      (if (not (equal? msg-in "quit"))
	  (let* ((dat        (decode msg-in))
		 (host-port  (alist-ref 'r dat)) ;; this is for the reverse req rep where the server is a client of the original client
		 (params     (alist-ref 'p dat))
		 (command    (let ((c (alist-ref 'c dat)))(if c (string->symbol c) #f)))
		 (all-good   (and host-port params command (hash-table-exists? *commands* command))))
	    (if all-good
		(let ((cmddat (make-qitem
			       command:   command
			       host-port: host-port
			       params:    params)))
		  (queue-push cmddat) 		;; put request into the queue
		  (nn-send soc "queued"))         ;; reply with "queued"
		(print "ERROR: BAD request " dat))
	    (loop (nn-recv soc)))))
    (nn-close soc)))
  
;;======================================================================
;; Gasket layer
;;======================================================================

(define rmt:open-create-db open-create-db)
(define (rmt:create-run . params)
  
  
