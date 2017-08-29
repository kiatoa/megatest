
;; Copyright 2006-2013, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

(use defstruct)
(use scsh-process)

(use srfi-18)
(use srfi-19)
(use refdb)

(use sql-de-lite srfi-1 posix regex regex-case srfi-69)
(declare (uses common))

(declare (uses configf))
(declare (uses margs))
(declare (uses megatest-version))

(include "megatest-fossil-hash.scm")
;;; please create this file before using sautherise. For sample file is avaliable sample-sauth-paths.scm. 
(include "sauth-paths.scm")
(include "sauth-common.scm")

;;
;; GLOBALS
;;
(define *verbosity* 1)
(define *logging* #f)
(define *exe-name* (pathname-file (car (argv))))
(define *sretrieve:current-tab-number* 0)
(define *args-hash* (make-hash-table))
(define sauthorize:help (conc "Usage: " *exe-name* " [action [params ...]]

  list                   		 			: list areas $USER's can access
  log                    		 			: get listing of recent activity.
  sauth  list-area-user <area code> 			: list the users that can access the area.
  sauth open <path> --group <grpname>                      : Open up an area. User needs to be the owner of the area to open it. 
              --code <unique short identifier for an area> 
              --retrieve|--publish [--additional-grps <comma separated unix grps requierd to get to the path>]
  sauth update <area code>  --retrieve|--publish             : update the binaries with the lates changes
  sauth grant <username> --area <area identifier>          : Grant permission to read or write to a area that is alrady opend up.    
             --expiration yyyy/mm/dd --retrieve|--publish 
             [--restrict <comma separated directory names> ]  
  sauth read-shell <area identifier>                       :  Open sretrieve shell for reading.  
  sauth write-shell <area identifier>                      :  Open spublish shell for writing.
   
Part of the Megatest tool suite.
Learn more at http://www.kiatoa.com/fossils/megatest

Version: " megatest-fossil-hash)) ;; "

;;======================================================================
;; RECORDS
;;======================================================================

;;======================================================================
;; DB
;;======================================================================

;; replace (strftime('%s','now')), with datetime('now'))
(define (sauthorize:initialize-db db)
  (for-each
   (lambda (qry)
     (exec (sql db qry)))
   (list 
    "CREATE TABLE IF NOT EXISTS actions
         (id           INTEGER PRIMARY KEY,
          cmd       TEXT NOT NULL,
          user_id      INTEGER NOT NULL,
          datetime     TIMESTAMP DEFAULT (datetime('now','localtime')),
          area_id      INTEGER NOT NULL,
          comment      TEXT DEFAULT '' NOT NULL,
          action_type  TEXT NOT NULL);"
        "CREATE TABLE IF NOT EXISTS users
         (id           INTEGER PRIMARY KEY,
          username     TEXT NOT NULL,
          is_admin     TEXT NOT NULL,
          datetime     TIMESTAMP DEFAULT (datetime('now','localtime'))
          );" 
          "CREATE TABLE IF NOT EXISTS areas
         (id           INTEGER PRIMARY KEY,
          basepath     TEXT NOT NULL,
          code         TEXT NOT NULL,
          exe_name     TEXT NOT NULL,
          required_grps TEXT DEFAULT '' NOT NULL,
          datetime     TIMESTAMP DEFAULT (datetime('now','localtime'))
          );" 
         "CREATE TABLE IF NOT EXISTS permissions
         (id              INTEGER PRIMARY KEY,
          access_type     TEXT NOT NULL,
          user_id         INTEGER NOT NULL,
          datetime        TIMESTAMP DEFAULT (datetime('now','localtime')),
          area_id         INTEGER NOT NULL,
          restriction     TEXT DEFAULT '' NOT NULL,
          expiration       TIMESTAMP DEFAULT NULL);"
    )))




(define (get-access-type args)
   (let loop ((hed (car args))
		 (tal (cdr args)))
                   (cond
                   ((equal? hed "--retrieve")
                      "retrieve") 
                   ((equal? hed "--publish")
                      "publish") 
                   ((equal? hed "--area-admin")
                      "area-admin")
                   ((equal? hed "--writer-admin")
                      "writer-admin")
                   ((equal? hed "--read-admin")
                      "read-admin")

                   ((null? tal)
                      #f) 
                   (else 
		  	(loop (car tal)(cdr tal))))))



;; check if user can gran access to an area
(define (can-grant-perm username access-type area)
   (let* ((isadmin (is-admin username))
          (is-area-admin (is-user "area-admin" username area ))
          (is-read-admin (is-user "read-admin" username area) )
          (is-writer-admin (is-user "writer-admin" username area) ) )
   (cond
   ((equal? isadmin  #t)
     #t)
   ((equal? is-area-admin #t ) 
     #t)
   ((and (equal? is-writer-admin #t ) (equal? access-type "retrieve"))
     #t)
   ((and (equal? is-read-admin #t ) (equal? access-type "retrieve"))
     #t)

   (else  
    #f))))

(define (sauthorize:list-areausers  area )
  (sauthorize:db-do  (lambda (db)
				     (print "Users having access to " area ":")
				     (query (for-each-row
					     (lambda (row)
                                               (let* ((exp-date (cadr row)))
                                                (if  (is-access-valid  exp-date)   
					        (apply print (intersperse row " | "))))))
					    (sql db (conc "SELECT users.username, permissions.expiration, permissions.access_type  FROM users, areas, permissions where permissions.user_id = users.id and permissions.area_id = areas.id and areas.code = '" area "'"))))))




; check if executable exists
(define (exe-exist exe access-type)
    (let* ((filepath (conc *exe-path* "/" access-type "/" exe)))
    ; (print filepath)
     (if (file-exists? filepath)
       #t
       #f)))

(define (copy-exe access-type exe-name group)
  (run-cmd "/bin/chmod" (list "g+w" (conc *exe-path* "/" access-type)))
  (let* ((spath (conc *exe-src*  "/s" access-type))
         (dpath (conc *exe-path* "/" access-type "/" exe-name)))
         (sauthorize:do-as-calling-user
        (lambda ()
            (run-cmd "/bin/cp" (list spath dpath )) 
            (if (equal? access-type "publish")
              (run-cmd "/bin/chmod" (list "u+s,o+rx" dpath))
              (begin
               (if (equal? group "none")
                 (run-cmd "/bin/chmod" (list "u+s,o+rx" dpath))
                 (begin   
                     (run-cmd "/bin/chgrp" (list group dpath))
                       (run-cmd "/bin/chmod" (list "g+s,o+rx" dpath))))))))
	(run-cmd "chmod" (list "g-w" (conc *exe-path* "/" access-type)))))

(define (get-exe-name path group)
   (let ((name ""))
   (sauthorize:do-as-calling-user
        (lambda ()
        (if (equal? (current-effective-user-id) (file-owner path)) 
          (set! name (conc (current-user-name) "_" group))
          (begin
            (print "You cannot open areas that you dont own!!")  
             (exit 1)))))
name))

(define (sauthorize:valid-unix-user username)
    (let* ((ret-val #f))
    (let-values (((inp oup pid)
              (process "/usr/bin/id" (list username))))
        (let loop ((inl (read-line inp)))
          (if (string? inl) 
          (if (string-contains inl  "No such user") 
            (set! ret-val #f)
             (set! ret-val #t)))   
          (if (eof-object? inl)
              (begin
                   (close-input-port inp)
                  (close-output-port oup))
            (loop (read-line inp)))))
            ret-val))


;check if a paths/codes are vaid and if area is alrady open  
(define (open-area group path code access-type other-grps)
   (let* ((exe-name (get-exe-name path group))
           (path-obj (get-obj-by-path path))
           (code-obj (get-obj-by-code-no-grp-validation code)))
           ;(print path-obj)   
          (cond
            ((not (null? path-obj))
                (if (equal? code (car path-obj))
                  (begin
                     (if (equal? exe-name (cadr path-obj))
                        (begin
                            (if (not (exe-exist exe-name  access-type))
                                 (copy-exe access-type exe-name group)
                                 (begin 
                                  (print "Area already open!!")
                                  (exit 1))))   
			(begin
                           (if (not (exe-exist exe-name  access-type))
                                 (copy-exe access-type exe-name group))
                           ;; update exe-name  in db 
                      (sauthorize:db-do   (lambda (db)
                         (sauthorize:db-qry db (conc "update areas set exe_name = '" exe-name "' where id = " (caddr path-obj)))))
                        )))
                   (begin
                       (print "Path " path " is registered with --code " (car path-obj) ". To open this area please execute following cmd: \n  sauthorize open " path " --group " group " --code " (car path-obj) " --" access-type )
                       (exit 1))))
                      
            ((not (null? code-obj))
                   (print "Code " code " is used for diffrent path. Please try diffrent value of --code" ) 
                   (exit 1))
            (else
               ; (print (exe-exist exe-name  access-type))
                (if (not (exe-exist exe-name  access-type))
                        (copy-exe access-type exe-name group))
                (sauthorize:db-do   (lambda (db)
               (print conc "insert into areas (code, basepath, exe_name, required_grps) values ('" code "', '" path "', '" exe-name "', '" other-grps "') ") 
             (sauthorize:db-qry db (conc "insert into areas (code, basepath, exe_name, required_grps) values ('" code "', '" path "', '" exe-name "', '" other-grps "') "))))))))

(define (user-has-open-perm user path access)
  (let* ((has-access #f)
         (eid (current-user-id)))
    (cond
     ((is-admin  user)
       (set! has-access #t ))
     ((and (is-read-admin  user) (equal? access "retrieve"))
       (set! has-access #t ))
     (else
        (print "User " user " does not have permission to open areas")))
        has-access))


;;check if user has group access
(define (is-group-washed req_grpid current-grp-list)
  (let loop ((hed (car current-grp-list))
		 (tal (cdr current-grp-list)))
                   (cond
                   ((equal? hed req_grpid)
                    #t)    
                   ((null? tal)
                      #f)
                   (else 
		  	(loop (car tal)(cdr tal))))))

;create executables with appropriate suids
(define (sauthorize:open user path group code access-type other-groups)
   (let* ((gpid (group-information group))
         (req_grpid (if (equal? group "none")
                      group 
                      (if (equal? gpid #f)
                           #f      
                     (caddr gpid))))
         (current-grp-list (get-groups))
         (valid-grp (if (equal? group "none")
                     group
                    (is-group-washed req_grpid current-grp-list))))
   (if (and (not (equal? group "none")) (equal? valid-grp #f ))
       (begin
       (print "Group " group " is not washed in the current xterm!!") 
       (exit 1)))) 
   (if (not (file-write-access? path))
     (begin
       (print "You can open areas owned by yourself. You do not have permissions to open path." path)
        (exit 1)))
   (if (user-has-open-perm user path access-type)
      (begin 
       ;(print "here")   
       (open-area group path code access-type other-groups)
       (sauthorize:grant user user code "2017/12/25"  "read-admin" "") 
       (sauthorize:db-do   (lambda (db)
             (sauthorize:db-qry db (conc "INSERT INTO actions (cmd,user_id,area_id,action_type ) VALUES ('sauthorize open " path " --code " code " --group " group " --" access-type "'," (car (get-user user)) "," (car (get-area code)) ", 'open' )"))))
         (print "Area has " path "  been opened for " access-type ))))

(define (sauthorize:update username exe area access-type)
  (let* ((parts (string-split exe "_"))
         (owner (car parts))
         (group (cadr parts))
         (gpid (group-information group))
         (req_grpid (if (equal? group "none")
                      group 
                      (if (equal? gpid #f)
                           #f      
                     (caddr gpid))))
 
         (current-grp-list (get-groups))
         (valid-grp (if (equal? group "none")
                     group
                    (is-group-washed req_grpid current-grp-list))))
         (if (not (equal? username owner))
            (begin
              (print "You cannot update " area ". Only " owner " can update this area!!") 
               (exit 1)))
          (copy-exe access-type exe group)
           (print "recording action..")    
          (sauthorize:db-do   (lambda (db)
             
             (sauthorize:db-qry db (conc "INSERT INTO actions (cmd,user_id,area_id,action_type ) VALUES ('sauthorize update " area " --" access-type "'," (car (get-user username)) "," (car (get-area area)) ", 'update' )"))))
         (print "Area has " area "  been update!!" )))

(define (sauthorize:grant auser guser area exp-date access-type restrict)
    ; check if user exist in db
    (let* ((area-obj (get-area area))
           (auser-obj (get-user auser)) 
           (user-obj (get-user guser)))
          
        (if (null? user-obj)
           (begin
            ;; is guser a valid unix user
            (if (not (sauthorize:valid-unix-user guser))
               (begin  
                (print "User " guser " is Invalid unix user!!")
                 (exit 1)))
            (sauthorize:db-do   (lambda (db)
             (sauthorize:db-qry db (conc "insert into users (username, is_admin) values ('" guser "', 'no') "))))
             (set! user-obj (get-user guser))))
        (let* ((perm-obj (get-perm (car user-obj) (car area-obj))))
          (if(null? perm-obj)
          (begin   
            ;; insert permissions
            (sauthorize:db-do   (lambda (db)
            (sauthorize:db-qry db (conc "insert into permissions (access_type, user_id, area_id, restriction, expiration ) values ('" access-type "', " (car user-obj) ", " (car area-obj) ", '" restrict "', '" exp-date "')")))))
          (begin 
             ;update permissions
             (sauthorize:db-do   (lambda (db)
             (sauthorize:db-qry db (conc "update permissions set access_type = '" access-type "' , restriction = '" restrict "', expiration =  '" exp-date "' where user_id = " (car user-obj) " and area_id = " (car area-obj)))))))
             (sauthorize:db-do   (lambda (db)
             (sauthorize:db-qry db (conc "INSERT INTO actions (cmd,user_id,area_id,action_type ) VALUES ('sauthorize grant " guser " --area " area " --expiration " exp-date " --" access-type " --restrict " restrict "'," (car auser-obj) "," (car area-obj) ", 'grant' )"))))  
             (print "Permission has been sucessfully granted to user " guser))))

(define (sauthorize:process-action  username action . args)
   (case (string->symbol action)
   ((grant)
      (if (< (length args) 6)
         (begin 
	     (print  "ERROR: Missing arguments; " (string-intersperse args ", "))
	     (exit 1)))
       (let* ((remargs     (args:get-args args '("--area" "--expiration" "--restrict") '() args:arg-hash 0))
              (guser     (car args))
	      (restrict         (or (args:get-arg "--restrict") ""))
              (area         (or (args:get-arg "--area") ""))  
              (exp-date        (or (args:get-arg "--expiration") ""))
              (access-type (get-access-type remargs)))
	; (print  "version " guser " restrict " restrict )
        ; (print "area " area " exp-date " exp-date " access-type " access-type)
        (cond
           ((equal? guser "")
              (print "Username not found!! Try \"sauthorize help\" for useage ")
               (exit 1))   
           ((equal? area "")
              (print "Area not found!! Try \"sauthorize help\" for useage ")
              (exit 1)) 
           ((equal? access-type #f)
              (print "Access type not found!! Try \"sauthorize help\" for useage ")
               (exit 1)) 
           ((equal? exp-date "")
              (print "Date of expiration not found!! Try \"sauthorize help\" for useage ")
              (exit 1)))
           (if (not (area-exists area))
              (begin
              (print "Area does not exisit!!")
              (exit 1)))   
           (if (can-grant-perm username access-type area)
	   (begin
             (print "calling sauthorize:grant ") 
              (sauthorize:grant username guser area exp-date access-type restrict))   
           (begin
              (print "User " username " does not have permission to grant permissions to area " area "!!")
              (exit 1)))))
       ((list-area-user)
          (if (not (equal? (length args) 1))
              (begin
              (print "Missing argument area code to list-area-user ") 
              (exit 1)))
           (let* ((area (car args)))
           (if (not (area-exists area))
              (begin
              (print "Area does not exisit!!")
              (exit 1))) 
                                
                (sauthorize:list-areausers  area )
              ))
      ((read-shell)
          (if (not (equal? (length args) 1))
              (begin
              (print "Missing argument area code to read-shell ") 
              (exit 1)))
           (let* ((area (car args))
                  (code-obj (get-obj-by-code area)))
           (if (or (null? code-obj)
                   (not (exe-exist (cadr code-obj)  "retrieve")))
              (begin
              (print "Area " area " is not open for reading!!")
              (exit 1))) 
              (sauthorize:do-as-calling-user
             (lambda ()
                (run-cmd (conc *exe-path* "/retrieve/" (cadr code-obj) ) (list "shell" area ))))))
      ((write-shell)
          (if (not (equal? (length args) 1))
              (begin
              (print "Missing argument area code to read-shell ") 
              (exit 1)))
           (let* ((area (car args))
                  (code-obj (get-obj-by-code area)))
           (if (or (null? code-obj)
                   (not (exe-exist (cadr code-obj)  "publish")))
              (begin
              (print "Area " area " is not open for Writing!!")
              (exit 1))) 
              (sauthorize:do-as-calling-user
             (lambda ()
                (run-cmd (conc *exe-path* "/publish/" (cadr code-obj) ) (list "shell" area))))))
      ((publish)
          (if (< (length args) 2)
              (begin
              (print "Missing argument to publish. \n publish <action> <area> [opts] ") 
              (exit 1)))
            
           (let* ((action (car args))
                  (area (cadr args))
                  (cmd-args (cddr args)) 
                  (code-obj (get-obj-by-code area)))
           ;(print "area " area)
           ;(print "code: " code-obj)  
           ;(print (exe-exist (cadr code-obj)  "publish")) 
           (if (or (null? code-obj)
                   (not (exe-exist (cadr code-obj)  "publish")))
              (begin
              (print "Area " area " is not open for writing!!")
              (exit 1)))
              ;(print "hear") 
              (sauthorize:do-as-calling-user
             (lambda ()
               ; (print  *exe-path* "/publish/" (cadr code-obj) action area cmd-args  )
                (run-cmd (conc *exe-path* "/publish/" (cadr code-obj) ) (append (list action area ) cmd-args))))))
      
     ((retrieve)
          (if (< (length args) 2)
              (begin
              (print "Missing argument to publish. \n publish <action> <area> [opts] ") 
              (exit 1)))
           (let* ((action (car args))
                  (area (cadr args))
                  (cmd-args (cddr args)) 
                  (code-obj (get-obj-by-code area)))
           (if (or (null? code-obj)
                   (not (exe-exist (cadr code-obj)  "retrieve")))
              (begin
              (print "Area " area " is not open for reading!!")
              (exit 1))) 
               (print (conc *exe-path* "/retrieve/" (cadr code-obj) " " action " " area " " (string-join cmd-args)))
              (sauthorize:do-as-calling-user
             (lambda ()
                (run-cmd (conc *exe-path* "/retrieve/" (cadr code-obj) ) (append (list action area ) cmd-args))))))

 
 
      ((open)
         (if (< (length args) 6)
              (begin
              (print "sauthorize open cmd takes 6 arguments!! \n Useage: sauthorize open <path> --group <grpname> --code <unique short identifier for an area> --retrieve|--publish") 
              (exit 1)))
         (let* ((remargs     (args:get-args args '("--group" "--code" "--additional-grps") '() args:arg-hash 0))
              (path     (car args))
	      (group         (or (args:get-arg "--group") ""))
              (area         (or (args:get-arg "--code") ""))
              (other-grps          (or (args:get-arg "--additional-grps") ""))     
              (access-type (get-access-type remargs)))
                
              (cond
                ((equal? path "")
                  (print "path not found!! Try \"sauthorize help\" for useage ")
                  (exit 1))   
                ((equal? area "")
                  (print "--code not found!! Try \"sauthorize help\" for useage ")
                  (exit 1)) 
                ((equal? access-type #f)
                  (print "Access type not found!! Try \"sauthorize help\" for useage ")
                  (exit 1)) 
                ((and (not (equal? access-type "publish")) 
                  (not (equal? access-type "retrieve")))
                  (print "Access type can be eiter --retrieve or --publish !! Try \"sauthorize help\" for useage ")
                  (exit 1)))
                ; (print other-grps) 
                (sauthorize:open username path group area access-type other-grps)))
         ((update)
            (if (< (length args) 2)
              (begin
              (print "sauthorize update cmd takes 2 arguments!! \n Useage: sauthorize update <area-code> --retrieve|--publish") 
              (exit 1)))
              (let* ((area (car args))
                     (code-obj (get-obj-by-code area))
                    (access-type (get-access-type (cdr args))))
               (if  (and (not (equal? access-type "publish")) (not (equal? access-type "retrieve")))
                  (begin 
                  (print "Access type can be --retrieve|--publish ")
                  (exit 1)))
              (if (or (null? code-obj)
                   (not (exe-exist (cadr code-obj)  access-type)))
              (begin
              (print "Area " area " is not open for reading!!")
              (exit 1))) 
              (sauthorize:update username (cadr code-obj) area access-type ))) 
         ((area-admin)
           (let* ((usr (car args))
                  (usr-obj (get-user usr))
                  (user-id (car (get-user username))))
           
                (if (is-admin  username)
                (begin
                  ; (print usr-obj) 
                  (if (null? usr-obj)
                    (begin
                        (sauthorize:db-do   (lambda (db)
              ;(print (conc "INSERT INTO users (username,is_admin) VALUES ( '" usr "', 'read-admin' )"))
             (sauthorize:db-qry db (conc "INSERT INTO users (username,is_admin) VALUES ( '" usr "', 'read-admin' )")))))
               (begin
                ; (print (conc "update users set is_admin = 'no' where id = " (car usr-obj) ))
                 (sauthorize:db-do   (lambda (db)
                (sauthorize:db-qry db (conc "update users set is_admin = 'read-admin' where id = " (car usr-obj)))))))
                (print "User " usr " is updated with area-admin access!"))
                (print "Admin only function"))
                (sauthorize:db-do   (lambda (db)
             (sauthorize:db-qry db (conc "INSERT INTO actions (cmd,user_id,area_id,action_type ) VALUES ('area-admin " usr " ', " user-id ",0, 'area-admin ')" )))))) 
          ((mk-admin)
           (let* ((usr (car args))
                  (usr-obj (get-user usr))
                  (user-id (car (get-user username))))
                (if (not (sauthorize:valid-unix-user usr))
               (begin  
                (print "User " usr " is Invalid unix user!!")
                 (exit 1)))

                (if (member  username  *super-users*)
                (begin
                  (if (null? usr-obj)
                    (begin
                        (sauthorize:db-do   (lambda (db)
                           (sauthorize:db-qry db (conc "INSERT INTO users (username,is_admin) VALUES ( '" usr "', 'yes' )")))))
               (begin
                 (sauthorize:db-do   (lambda (db)
                (sauthorize:db-qry db (conc "update users set is_admin = 'yes' where id = " (car usr-obj)))))))
                (print "User " usr " is updated with admin access!"))
                (print "Super-Admin only function"))
                (sauthorize:db-do   (lambda (db)
             (sauthorize:db-qry db (conc "INSERT INTO actions (cmd,user_id,area_id,action_type ) VALUES ('mk-admin " usr " ', " user-id ",0, 'mk-admin ')" )))))) 

         ((register-log)
            (if (< (length args) 4)
                (print "Invalid arguments"))
             ;(print args)
             (let* ((cmd-line (car args))
                     (user-id (cadr args))
                     (area-id (caddr args))
                     (user-obj (get-user username))
                      (cmd (cadddr args)))
                
               (if (and (not (null? user-obj)) (equal? user-id (number->string(car user-obj))))
                (begin 
                (sauthorize:db-do   (lambda (db)
             (sauthorize:db-qry db (conc "INSERT INTO actions (cmd,user_id,area_id,action_type ) VALUES ('" cmd-line"', " user-id "," area-id ", '" cmd "')" )))))
                (print "You ar not authorised to run this cmd")

)))     

       
      (else (print 0 "Unrecognised command " action))))
  
(define (main)
  (let* ((args      (argv))
	 (prog      (car args))
	 (rema      (cdr args))
         (username     (current-user-name)))
    ;; preserve the exe data in the config file
    (cond
     ;; one-word commands
     ((eq? (length rema) 1)
      (case (string->symbol (car rema))
	((help -h -help --h --help)
	 (print sauthorize:help))
	((list)
            
          (sauthorize:db-do  (lambda (db)
				     (print "My Area accesses: ")
				     (query (for-each-row
					     (lambda (row)
                                               (let* ((exp-date (car row)))
                                                (if  (is-access-valid  exp-date)     
					           (apply print (intersperse (cdr row) " | "))))))
					    (sql db (conc "SELECT permissions.expiration, areas.basepath, areas.code, permissions.access_type  FROM users, areas, permissions where permissions.user_id = users.id and permissions.area_id = areas.id and users.username = '" username "'"))))))
         
	((log)
	 (sauthorize:db-do  (lambda (db)
				     (print "Logs : ")
				     (query (for-each-row
					     (lambda (row)
                                                   
					       (apply print (intersperse row " | "))))
					    (sql db "SELECT actions.cmd, users.username, actions.action_type, actions.datetime, areas.code  FROM actions, users, areas where actions.user_id = users.id and actions.area_id = areas.id ")))))
	(else
	 (print "ERROR: Unrecognised command. Try \"sauthorize help\""))))
     ;; multi-word commands
     ((null? rema)(print sauthorize:help))
     ((>= (length rema) 2)
      (apply sauthorize:process-action username (car rema)(cdr rema)))
     (else (debug:print 0 "ERROR: Unrecognised command. Try \"sauthorize help\"")))))

(main)


      
