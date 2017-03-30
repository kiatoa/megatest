
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
            ;(print  "calling proc " proc "db path " dbpath )
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
  ;(print qry)
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


(define (run-cmd cmd arg-list)
  ; (print (current-effective-user-id))
   ;(handle-exceptions
;	     exn
;	     (print 0 "ERROR: failed to run script " cmd " with params " arg-list " " (exn assert))
	     (let ((pid (process-run cmd arg-list)))
	       (process-wait pid))
)
;)


(define (regster-log inl usr-id  area-id  cmd)
  (sauth-common:shell-do-as-adm
        (lambda ()
         (sauthorize:db-do   (lambda (db)
             (sauthorize:db-qry db (conc "INSERT INTO actions (cmd,user_id,area_id,action_type ) VALUES ('sretrieve " inl "'," usr-id ","  area-id ", 'cat' )")))))))

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


;;check if a user is an read-admin
(define (is-read-admin username)
   (let* ((admin #f))
    (sauthorize:db-do  (lambda (db)
        (let* ((data-row (query fetch (sql db (conc "SELECT users.is_admin FROM  users where users.username = '" username "'")))))
        (if (not (null? data-row))
             (let ((col  (car data-row)))
             (if (equal? col "read-admin")
                   (set! admin #t)))))))  	        
admin))


;;check if user has specifc role for a area
(define (is-user role username area)
  (let* ((has-access #f))
    (sauthorize:db-do  (lambda (db)
        (let* ((data-row (query fetch (sql db (conc "SELECT  permissions.access_type, permissions.expiration FROM  users ,  areas, permissions where permissions.user_id = users.id and permissions.area_id = areas.id and users.username = '" username "' and areas.code = '" area "'")))))
        (if (not (null? data-row))
           (begin
               (let* ((access-type  (car data-row))
                    (exdate (cadr data-row)))
               (if (not (null? exdate)) 
               (begin 
                  (let ((valid (is-access-valid  exdate)))
                   ;(print valid) 
                  (if (and (equal? access-type role)
                        (equal? valid #t))
                   (set! has-access #t))))
                (print "Access expired"))))))))
 ;(print has-access)
has-access))

(define (is-access-valid exp-str)
    (let* ((ret-val #f )
           (date-parts  (string-split exp-str "/"))
           (yr (string->number (car date-parts)))
           (month (string->number(car (cdr date-parts)))) 
           (day (string->number(caddr date-parts)))
           (exp-date (make-date 0 0 0 0 day month yr )))
             ;(print  exp-date)
             ;(print (current-date))   
            (if (> (date-compare exp-date  (current-date)) 0)
             (set! ret-val #t))
   ;(print ret-val)
   ret-val))


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

(define (get-restrictions base-path usr)
(let* ((user-defined '()))
    (sauthorize:db-do  (lambda (db)
          (let* ((data-row (query fetch (sql db (conc "SELECT  restriction FROM areas, users, permissions where  areas.id = permissions.area_id and users.id =  permissions.user_id and  users.username = '" usr "' and areas.basepath = '" base-path "'")))))
         ;(print data-row) 
         (set!  user-defined data-row))))
    ;   (print user-defined)
  (if (null? user-defined)
      ""
      (car user-defined))))


(define (get-obj-by-path path)
   (let* ((obj '()))
    (sauthorize:db-do  (lambda (db)
        (let* ((data-row (query fetch (sql db (conc "SELECT  code,exe_name, id, basepath FROM  areas where areas.basepath = '" path "'")))))
         (set!  obj data-row))))
obj))

(define (get-obj-by-code code )
  (let* ((obj '()))
    (sauthorize:db-do  (lambda (db)
        (let* ((data-row (query fetch (sql db (conc "SELECT  code, exe_name,  id, basepath, required_grps  FROM  areas where areas.code = '" code "'")))))
         (set!  obj data-row)
        )))
    (if (not (null? obj))
          (begin
          (let* ((req-grp (caddr (cddr obj))))
            (sauthorize:do-as-calling-user
             (lambda ()
 (sauth-common:check-user-groups req-grp))))))
obj))

(define (sauth-common:check-user-groups req-grp)
(let* ((current-groups  (get-groups) )
        (req-grp-list (string-split req-grp ",")))
        ;(print req-grp-list)
        (for-each (lambda (grp)
	  (let ((grp-info (group-information grp)))
               ;(print grp-info " " grp)
               (if (not (equal? grp-info #f))
               (begin
                 (if (not (member  (caddr grp-info) current-groups))
                  (begin 
                    (sauth:print-error (conc "Please wash " grp " group in your xterm!! " ))
                     (exit 1)))))))
	     req-grp-list)))

(define (get-obj-by-code-no-grp-validation code )
  (let* ((obj '()))
    (sauthorize:db-do  (lambda (db)
        (let* ((data-row (query fetch (sql db (conc "SELECT  code, exe_name,  id, basepath  FROM  areas where areas.code = '" code "'")))))
         (set!  obj data-row))))
;(print obj)
obj))




;; function to validate the users input for target path and resolve the path
;; TODO: Check for restriction in subpath 
(define (sauth-common:resolve-path  new current allowed-sheets)
   (let* ((target-path (append  current (string-split new "/")))
          (target-path-string (string-join target-path "/"))
          (normal-path (normalize-pathname target-path-string))
          (normal-list (string-split normal-path "/"))
           (ret '()))
   (if (string-contains   normal-path "..")
    (begin
      (print "ERROR: Path  " new " resolved outside target area ")
      #f)
    (if(equal? normal-path ".")
      ret  
    (if (not (member  (car normal-list) allowed-sheets))
      (begin
      (print "ERROR: Permision denied to  " new )
       #f)
    normal-list)))))

(define (sauth-common:get-target-path base-path-list ext-path top-areas base-path)
  (let* ((resolved-path (sauth-common:resolve-path ext-path base-path-list top-areas ))
          (usr (current-user-name) ) )
          (if (not (equal? resolved-path #f))
           (if (null? resolved-path) 
             #f
           (let* ((sheet (car resolved-path))
                   (restricted-areas (get-restrictions base-path usr))
                   (restrictions (conc ".*" (string-join (string-split restricted-areas ",") ".*|.*") ".*"))
           	   (target-path (if (null? (cdr resolved-path)) 
                                     base-path 
                                     (conc base-path "/" (string-join (cdr resolved-path) "/")))))
                    
	              
                           (if (and (not (equal? restricted-areas "" ))
                             (string-match (regexp  restrictions) target-path)) 
                           (begin
                              (sauth:print-error "Access denied to " (string-join resolved-path "/"))
                              ;(exit 1)   
                            #f)
                             target-path)
                            
))
             #f)))

(define (sauth-common:shell-ls-cmd base-path-list ext-path top-areas base-path tail-cmd-list)
    (if (and (null? base-path-list) (equal? ext-path "") )
      (print (string-intersperse top-areas " "))
  (let* ((resolved-path (sauth-common:resolve-path ext-path base-path-list top-areas )))
           ;(print resolved-path)
           (if (not (equal? resolved-path #f))
           (if (null? resolved-path) 
             (print (string-intersperse top-areas " "))
           (let* ((target-path (sauth-common:get-target-path  base-path-list  ext-path top-areas base-path)))
                (print target-path)
                (if (not (equal? target-path #f))
                (begin 
                (cond
		  ((null? tail-cmd-list)
		     (run (pipe
      	      	      (ls "-lrt" ,target-path))))
		  ((not (equal? (car tail-cmd-list) "|"))
                         (print "ls cmd cannot accept " (string-join tail-cmd-list) " as an argument!!"))
                  (else  
                    (run (pipe
      	      	      (ls "-lrt" ,target-path)
                      (begin (system (string-join (cdr tail-cmd-list))))))))))))))))

(define (sauth:print-error msg)
  (with-output-to-port (current-error-port)
	(lambda ()
	       (print (conc "ERROR: " msg)))))

