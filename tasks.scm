;; Copyright 2006-2011, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

;;  strftime('%m/%d/%Y %H:%M:%S','now','localtime')

(use sqlite3 srfi-1 posix regex regex-case srfi-69 dot-locking)
(import (prefix sqlite3 sqlite3:))

(declare (unit runs))
(declare (uses db))
(declare (uses common))
(declare (uses items))
(declare (uses runconfig))

(include "common_records.scm")
(include "key_records.scm")
(include "db_records.scm")
(include "run_records.scm")
(include "task_records.scm")

;;======================================================================
;; Tasks and Task monitors
;;======================================================================


;;======================================================================
;; Tasks
;;======================================================================



;;======================================================================
;; Task Monitors
;;======================================================================

(define (tasks:register-monitor db)
  (let* ((pid (current-process-id))
	 (hostname (get-host-name))
	 (userinfo (user-information (current-user-id)))
	 (username (car userinfo)))
    (sqlite3:execute db "INSERT INTO monitors (pid,start_time,last_update,hostname,username) VALUES (?,strftime('%s','now'),strftime('%s','now'),?,?);"
		     pid hostname username)))

(define (tasks:get-num-alive-monitors db)
  (let ((res 0))
    (sqlite3:for-each-row 
     (lambda (count)
       (set! res count))
     db
     "SELECT count(id) FROM monitors WHERE last_update < (strftime('%s','now') - 300) AND username=?;"
     (car (user-information (current-user-id))))
    res))

;; return one task from those who are 'new' OR 'waiting' AND more than 10sec old
;;
(define (tasks:snag-a-task db)
  (let ((res #f))
    (with-transaction 
     db
     (lambda ()
       (sqlite3:for-each-row
	(lambda (id . rem)
	  (set! res (apply vector id rem)))
	db
	"SELECT id,action,owner,state,target,name,test,item,creation_time,exectution_time 
           FROM tasks_queue
             WHERE 
                state='new' OR (state='waiting' AND 
                                last_update+10 > strftime('%s','now'))
             LIMIT 1;")
       (if res ;; yep, have work to be done
	   (begin
	     (sqlite3:execute db "UPDATE tasks_queue SET state='inprogress' WHERE id=?;"
			      (tasks:task-get-id res))
	     res))))))

(define (tasks:start-monitor db)
  (if (> (tasks:get-num-alive-monitors db) 2) ;; have two running, no need for more
      (debug:print 1 "INFO: Not starting monitor, already have more than two running")
      (let* ((megatestdb     (conc *toppath* "/megatest.db"))
	     (last-db-update 0)) ;; (file-modification-time megatestdb)))
	(task:register-monitor db)
	(let loop ((count 0))
	  ;; if the db has been modified we'd best look at the task queue
	  (let ((modtime (file-modification-time megatestdb)))
	    (if (> modtime last-db-update)
		(let* ((task   (tasks:snag-a-task db))
		       (action (if task (tasks:task-get-action task) #f)))
		  (if action
		      (case (string->symbol action)
			((run)       (tasks:start-run   db task))
			((remove)    (tasks:remove-runs db task))
			((lock)      (tasks:lock-runs   db task))
			((monitor)   (tasks:start-monitor db task))
			((rollup)    (tasks:rollup-runs db task))
			((updatemeta)(tasks:update-meta db task))
			((kill)      (tasks:kill-monitors db task))))
		  ;; WARNING: Possible race conditon here!!
		  ;; should this update be immediately after the task-get-action call above?
		  (set! modtime (file-modification-time megatestdb)))))
	  (loop (+ count 1))))))


		  
	
	