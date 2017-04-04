;;======================================================================
;; Copyright 2017, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.
;;======================================================================

(declare (unit pgdb))
(declare (uses configf))

;; I don't know how to mix compilation units and modules, so no module here.
;;
;; (module pgdb
;;     (
;;      open-pgdb
;;      )
;; 
;; (import scheme)
;; (import data-structures)
;; (import chicken)

(use typed-records (prefix dbi dbi:))

;; given a configdat lookup the connection info and open the db
;;
(define (pgdb:open configdat #!key (dbname #f))  
  (let ((pgconf (or (args:get-arg "-pgsync") (configf:lookup configdat "ext-sync" (or dbname "pgdb")))))
    (if pgconf
	(let* ((confdat (map (lambda (conf-item)
			       (let ((parts (string-split conf-item ":")))
				 (if (> (length parts) 1)
				     (let ((key (car parts))
					   (val (cadr parts)))
				       (cons (string->symbol key) val))
				     (begin
				       (print "ERROR: Bad config setting " conf-item ", should be key:val")
				       `(,(string->symbol (car parts)) . #f)))))
			     (string-split pgconf)))
	       (dbtype   (string->symbol (or (alist-ref 'dbtype confdat) "pg"))))
	  (if (alist-ref 'dbtype confdat)
	      (dbi:open dbtype (alist-delete 'dbtype confdat))))
	#f)))

;;======================================================================
;;  A R E A S
;;======================================================================

(defstruct area id area-name area-path last-update)

(define (pgdb:add-area dbh area-name area-path)
  (dbi:exec dbh "INSERT INTO areas (area_name,area_path) VALUES (?,?)" area-name area-path))

(define (pgdb:get-areas dbh)
  ;; (map
  ;;  (lambda (row)
  ;;    (print "row: " row))
  (dbi:get-rows dbh "SELECT id,area_name,area_path,last_sync FROM areas;")) ;; )

;; given an area_path get the area info
;;
(define (pgdb:get-area-by-path dbh area-path)
  (dbi:get-one-row dbh "SELECT id,area_name,area_path,last_sync FROM areas WHERE area_path=?;" area-path))

(define (pgdb:write-sync-time dbh area-info new-sync-time)
  (let ((area-id (vector-ref area-info 0)))
    (dbi:exec dbh "UPDATE areas SET last_sync=? WHERE id=?;" new-sync-time area-id)))

;;======================================================================
;;  T A R G E T S
;;======================================================================

;; Given a target-spec, return the id. Should probably handle this with a join...
;; if target-spec not found, create a record for it.
;;
(define (pgdb:get-ttype dbh target-spec)
  (let ((spec-id (dbi:get-one dbh "SELECT id FROM ttype WHERE target_spec=?;" target-spec)))
    (or spec-id
	(if (handle-exceptions
		exn
		(begin
		  (print-call-chain)
		  (debug:print 0 *default-log-port* "ERROR: cannot create ttype entry, " ((condition-property-accessor 'exn 'message) exn))
		  #f)
	      (dbi:exec dbh "INSERT INTO ttype (target_spec) VALUES (?);" target-spec))
	    (pgdb:get-ttype dbh target-spec)))))

;;======================================================================
;;  R U N S
;;======================================================================

;; given a target spec id, target and run-name return the run-id
;; if no run found return #f
;;
(define (pgdb:get-run-id dbh spec-id target run-name)
  (dbi:get-one dbh "SELECT id FROM runs WHERE ttype_id=? AND target=? AND run_name=?;"
	       spec-id target run-name))

;; given a run-id return all the run info
;;
(define (pgdb:get-run-info dbh run-id) ;; to join ttype or not?
  (dbi:get-one-row
   dbh   ;; 0    1       2       3      4     5      6       7        8         9         10          11         12
   "SELECT id,target,ttype_id,run_name,state,status,owner,event_time,comment,fail_count,pass_count,last_update,area_id
       FROM runs WHERE id=?;" run-id))

;; refresh the data in a run record
;;
(define (pgdb:refresh-run-info dbh run-id state status owner event-time comment fail-count pass-count) ;; area-id)
  (dbi:exec
   dbh
   "UPDATE runs SET
      state=?,status=?,owner=?,event_time=?,comment=?,fail_count=?,pass_count=?
     WHERE id=?;"
   state status owner event-time comment fail-count pass-count run-id))

;; given all needed info create run record
;;
(define (pgdb:insert-run dbh ttype-id target run-name state status owner event-time comment fail-count pass-count)
  (dbi:exec
   dbh
   "INSERT INTO runs (ttype_id,target,run_name,state,status,owner,event_time,comment,fail_count,pass_count)
      VALUES (?,?,?,?,?,?,?,?,?,?);"
    ttype-id target run-name state status owner event-time comment fail-count pass-count))

;;======================================================================
;;  T E S T S
;;======================================================================

;; given run-id, test_name and item_path return test-id
;;
(define (pgdb:get-test-id dbh run-id test-name item-path)
  (dbi:get-one
   dbh
   "SELECT id FROM tests WHERE run_id=? AND test_name=? AND item_path=?;"
   run-id test-name item-path))

;; create new test record
;;
(define (pgdb:insert-test dbh run-id test-name item-path state status host cpuload diskfree uname run-dir log-file run-duration comment event-time archived)
  (dbi:exec
   dbh
   "INSERT INTO tests (run_id,test_name,item_path,state,status,host,cpuload,diskfree,uname,rundir,final_logf,run_duration,comment,event_time,archived)
       VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);"

   run-id  test-name item-path    state   status     host  cpuload diskfree uname
   run-dir log-file  run-duration comment event-time archived))

;; update existing test record
;;
(define (pgdb:update-test dbh test-id run-id test-name item-path state status host cpuload diskfree uname run-dir log-file run-duration comment event-time archived)
  (dbi:exec
   dbh
   "UPDATE tests SET
      run_id=?,test_name=?,item_path=?,state=?,status=?,host=?,cpuload=?,diskfree=?,uname=?,rundir=?,final_logf=?,run_duration=?,comment=?,event_time=?,archived=?
    WHERE id=?;"

   run-id  test-name item-path    state   status     host  cpuload diskfree uname
   run-dir log-file  run-duration comment event-time archived test-id))

(define (pgdb:get-tests dbh target-patt)
  (dbi:get-rows
   dbh
   "SELECT t.id,t.run_id,t.test_name,t.item_path,t.state,t.status,t.host,t.cpuload,t.diskfree,t.uname,t.rundir,t.final_logf,t.run_duration,t.comment,t.event_time,t.archived,
           r.id,r.target,r.ttype_id,r.run_name,r.state,r.status,r.owner,r.event_time,r.comment
     FROM tests AS t INNER JOIN runs AS r ON t.run_id=r.id
      WHERE r.target LIKE ?;" target-patt))

(define (pgdb:get-stats-given-type-target dbh ttype-id target-patt)
  (dbi:get-rows
   dbh
   ;;    "SELECT COUNT(t.id),t.status,r.target FROM tests AS t INNER JOIN runs AS r ON t.run_id=r.id
   ;;         WHERE t.state='COMPLETED' AND ttype_id=? AND r.target LIKE ? GROUP BY r.target,t.status;"
   "SELECT r.target,COUNT(*) AS total,
                    SUM(CASE WHEN t.status='PASS' THEN 1 ELSE 0 END) AS pass,
                    SUM(CASE WHEN t.status='FAIL' THEN 1 ELSE 0 END) AS fail,
                    SUM(CASE WHEN t.status IN ('PASS','FAIL') THEN 0 ELSE 1 END) AS other
            FROM tests AS t INNER JOIN runs AS r ON t.run_id=r.id
            WHERE t.state='COMPLETED' AND ttype_id=? AND r.target LIKE ? GROUP BY r.target;"
   ttype-id target-patt))

(define (pgdb:get-stats-given-target dbh target-patt)
  (dbi:get-rows
   dbh
   ;;    "SELECT COUNT(t.id),t.status,r.target FROM tests AS t INNER JOIN runs AS r ON t.run_id=r.id
   ;;         WHERE t.state='COMPLETED' AND ttype_id=? AND r.target LIKE ? GROUP BY r.target,t.status;"
   "SELECT r.target,COUNT(*) AS total,
                    SUM(CASE WHEN t.status='PASS' THEN 1 ELSE 0 END) AS pass,
                    SUM(CASE WHEN t.status='FAIL' THEN 1 ELSE 0 END) AS fail,
                    SUM(CASE WHEN t.status IN ('PASS','FAIL') THEN 0 ELSE 1 END) AS other
            FROM tests AS t INNER JOIN runs AS r ON t.run_id=r.id
            WHERE t.state='COMPLETED' AND r.target LIKE ? GROUP BY r.target;"
   target-patt))

(define (pgdb:get-target-types dbh)
  (dbi:get-rows dbh "SELECT id,target_spec FROM ttype;"))

;; 
(define (pgdb:get-targets dbh target-patt)
  (let ((ttypes (pgdb:get-target-types dbh)))
    (map
     (lambda (ttype-dat)
       (let ((tt-id (vector-ref ttype-dat 0))
	     (ttype (vector-ref ttype-dat 1)))
	 (cons ttype
	       (dbi:get-rows 
		dbh
		"SELECT DISTINCT target FROM runs WHERE target LIKE ? AND ttype_id=?;" target-patt tt-id))
	 ))
     ttypes)))

(define (pgdb:get-targets-of-type dbh ttype-id target-patt)
  (dbi:get-rows dbh "SELECT DISTINCT target FROM runs WHERE target LIKE ? AND ttype_id=?;" target-patt ttype-id))

(define (pgdb:get-runs-by-target dbh targets run-patt)
   (dbi:get-rows dbh "SELECT r.run_name, t.test_name, t.status, t.item_path, t.id, t.rundir, t.final_logf FROM runs as r INNER JOIN tests AS t ON t.run_id=r.id  
                          WHERE t.state='COMPLETED' AND r.target like ? AND  r.run_name like ?;" targets run-patt)
)

(define (pgdb:get-test-by-id dbh id)
  (dbi:get-rows dbh "SELECT t.test_name, t.item_path, t.rundir, t.final_logf FROM runs as r INNER JOIN tests AS t ON t.run_id=r.id  
                          WHERE t.id = ?;" id)
)

;;======================================================================
;;  V A R I O U S   D A T A   M A S S A G E   R O U T I N E S
;;======================================================================

;; probably want to move these to a different model file

;; create a hash of hashes with keys extracted from all-parts
;; using row-or-col to choose row or column
;;   ht{row key}=>ht{col key}=>data
;;
;; fnum is the field number in the tuples to be split
;;
(define (pgdb:coalesce-runs dbh runs all-parts row-or-col fnum)
  (let* ((data  (make-hash-table)))
    ;;	 (rnums (
    ;; for now just do first => remainder
    (for-each
     (lambda (run)
       (let* ((target (vector-ref run fnum))
	      (parts  (string-split target "/"))
	      (first  (car parts))
	      (rest   (string-intersperse (cdr parts) "/"))
	      (coldat (hash-table-ref/default data first #f)))
	 (if (not coldat)(let ((newht (make-hash-table)))
			   (hash-table-set! data first newht)
			   (set! coldat newht)))
	 (hash-table-set! coldat rest run)))
     runs)
    data))

(define (pgdb:runs-to-hash runs )
  (let* ((data  (make-hash-table)))
    (for-each
     (lambda (run)
       (let* ((run-name (vector-ref run 0))
	      (test (conc (vector-ref run 1) ":" (vector-ref run 3)))
	      (coldat (hash-table-ref/default data run-name #f)))
	 (if (not coldat)(let ((newht (make-hash-table)))
			   (hash-table-set! data run-name newht)
			   (set! coldat newht)))
	 (hash-table-set! coldat test run)))
     runs)
    data))
