
;; Create the sqlite db
(define (sauthorize:db-do proc) 
      (if (or (not *db-path*)
              (not (file-exists? *db-path*))) 
	(begin
	  (print 0 "[database]\nlocation " *db-path* " \n\n Is missing from the config file!")
	  (exit 1)))
    (if (and *db-path*
	     (directory? *db-path*)
	     (file-read-access? *db-path*))
	(let* ((dbpath    (conc *db-path* "/sauthorize.db"))
	       (writeable (file-write-access? dbpath))
	       (dbexists  (file-exists? dbpath)))
	  (handle-exceptions
	   exn
	   (begin
	     (debug:print 2 "ERROR: problem accessing db " dbpath
			  ((condition-property-accessor 'exn 'message) exn))
	     (exit 1))
          ;  (print  "calling proc " proc "db path " dbpath )
	   (call-with-database
            dbpath
	    (lambda (db)
	       ;(print 0 "calling proc " proc " on db " db)
	      (set-busy-handler! db (busy-timeout 10000)) ;; 10 sec timeout
	      (if (not dbexists)(sauthorize:initialize-db db))
	      (proc db)))))
	(print 0 "ERROR: invalid path for storing database: " *db-path*)))

;;execute a query
(define (sauthorize:db-qry db qry)
  (exec (sql db  qry)))


(define (sauthorize:do-as-calling-user proc)
  (let ((eid (current-effective-user-id))
        (cid (current-user-id)))
    (if (not (eq? eid cid)) ;; running suid
            (set! (current-effective-user-id) cid))
     ;(print 0 "cid " cid " eid:" eid)
    (proc)
    (if (not (eq? eid cid))
        (set! (current-effective-user-id) eid))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Check user types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;check if a user is an admin
(define (is-admin username)
   (let* ((admin #f))
    (sauthorize:db-do  (lambda (db)
        (let* ((data-row (query fetch (sql db (conc "SELECT users.is_admin FROM  users where users.username = '" username "'")))))
        (if (not (null? data-row))
             (let ((col  (car data-row)))
             (if (equal? col "yes")
                   (set! admin #t)))))))  	        
admin))

;;check if user has specifc role for a area
(define (is-user role username area)
  (let* ((has-access #f))
    (sauthorize:db-do  (lambda (db)
        (let* ((data-row (query fetch (sql db (conc "SELECT  permissions.access_type, permissions.expiration FROM  users ,  areas, permissions where permissions.user_id = users.id and permissions.area_id = areas.id and users.username = '" username "' and areas.code = '" area "'")))))
        (if (not (null? data-row))
             (let* ((access-type  (car data-row))
                    (exdate (cadr data-row)))
               (if (not (null? exdate)) 
               (begin 
                  (let ((valid (is-access-valid  exdate)))
                   ;(print valid) 
                  (if (and (equal? access-type role)
                        (equal? valid #t))
                   (set! has-access #t))))
                (print "Access expired")))))))
has-access))

;check if area exists
(define (area-exists area)
   (let* ((area-defined #f))
    (sauthorize:db-do  (lambda (db)
        (let* ((data-row (query fetch (sql db (conc "SELECT  id FROM  areas where areas.code = '" area "'")))))
           (if (not (null? data-row))
                 (set! area-defined #t)))))
area-defined))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Get Record from database
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;gets area id by code 
(define (get-area area)
   (let* ((area-defined '()))
    (sauthorize:db-do  (lambda (db)
        (let* ((data-row (query fetch (sql db (conc "SELECT  id FROM  areas where areas.code = '" area "'")))))
          (set!  area-defined data-row))))
area-defined))

;get id of users table by user name 
(define (get-user user)
  (let* ((user-defined '()))
    (sauthorize:db-do  (lambda (db)
        (let* ((data-row (query fetch (sql db (conc "SELECT  id FROM  users where users.username = '" user "'")))))
          (set!  user-defined data-row))))
user-defined))

;get permissions id by userid and area id 
(define (get-perm userid areaid)
  (let* ((user-defined '()))
    (sauthorize:db-do  (lambda (db)
          (let* ((data-row (query fetch (sql db (conc "SELECT  id FROM  permissions where user_id = " userid " and area_id = " areaid)))))
         (set!  user-defined data-row))))

user-defined))

