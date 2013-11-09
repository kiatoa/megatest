;; Copyright 2006-2011, Matthew Welland.
;;
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;;
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

;; (require-extension synch sqlite3 posix srfi-13 srfi-1 utils regex)
(use sqlite3 srfi-1 posix regex srfi-69 srfi-13 posix-extras)
(import (prefix sqlite3 sqlite3:))

(declare (unit filedb))

(include "fdb_records.scm")
;; (include "settings.scm")

(define (filedb:open-db dbpath)
  (let* ((fdb      (make-filedb:fdb))
	 (dbexists (file-exists? dbpath))
	 (db (sqlite3:open-database dbpath)))
    (filedb:fdb-set-db!        fdb db)
    (filedb:fdb-set-dbpath!    fdb dbpath)
    (filedb:fdb-set-pathcache! fdb (make-hash-table))
    (filedb:fdb-set-idcache!   fdb (make-hash-table))
    (filedb:fdb-set-partcache! fdb (make-hash-table))
    ;(sqlite3:set-busy-timeout! db 1000000)
    (if (not dbexists)
	(begin
	  (sqlite3:execute db "PRAGMA synchronous = OFF;")
	  (sqlite3:execute db "CREATE TABLE names (id INTEGER PRIMARY KEY,name TEST);") ;; for future use - change path in paths table to path_id
	  (sqlite3:execute db "CREATE INDEX name_index ON names (name);")
	  ;; NB// We store a useful subset of file attributes but do not attempt to store all
	  (sqlite3:execute db "CREATE TABLE paths (id        INTEGER PRIMARY KEY,
                                                   path      TEXT,
                                                   parent_id INTEGER,
                                                   mode      INTEGER DEFAULT -1,
                                                   uid       INTEGER DEFAULT -1,
                                                   gid       INTEGER DEFAULT -1,
                                                   size      INTEGER DEFAULT -1,
                                                   mtime     INTEGER DEFAULT -1);")
	  (sqlite3:execute db "CREATE INDEX path_index ON paths (path,parent_id);")
	  (sqlite3:execute db "CREATE TABLE bases (id INTEGER PRIMARY KEY,base TEXT,                  updated TIMESTAMP);")))
    fdb))

(define (filedb:finalize-db! fdb)
  (sqlite3:finalize! (filedb:fdb-get-db fdb)))

(define (filedb:get-current-time-string)
  (string-chomp (time->string (seconds->local-time (current-seconds)))))

(define (filedb:get-base-id db path)
  (let ((stmt   (sqlite3:prepare db "SELECT id FROM bases WHERE base=?;"))
        (id-num #f))
    (sqlite3:for-each-row 
     (lambda (num) (set! id-num num)) stmt path)
    (sqlite3:finalize! stmt)
    id-num))

(define (filedb:get-path-id db path parent)
  (let ((stmt   (sqlite3:prepare db "SELECT id FROM paths WHERE path=? AND parent_id=?;"))
        (id-num #f))
    (sqlite3:for-each-row 
     (lambda (num) (set! id-num num)) stmt path parent)
    (sqlite3:finalize! stmt)
    id-num))

(define (filedb:add-base db path)
  (let ((existing (filedb:get-base-id db path)))
    (if existing #f
        (begin
          (sqlite3:execute db "INSERT INTO bases (base,updated) VALUES (?,?);" path (filedb:get-current-time-string))))))

;; index 	value 	field 	notes
;; 0 	inode number 	st_ino 	
;; 1 	mode 	st_mode 	bitfield combining file permissions and file type
;; 2 	number of hard links 	st_nlink 	
;; 3 	UID of owner 	st_uid 	as with file-owner
;; 4 	GID of owner 	st_gid 	
;; 5 	size 	st_size 	as with file-size
;; 6 	access time 	st_atime 	as with file-access-time
;; 7 	change time 	st_ctime 	as with file-change-time
;; 8 	modification time 	st_mtime 	as with file-modification-time
;; 9 	parent device ID 	st_dev 	ID of device on which this file resides
;; 10 	device ID 	st_rdev 	device ID for special files (i.e. the raw major/minor number)
;; 11 	block size 	st_blksize 	
;; 12 	number of blocks allocated 	st_blocks 	

(define (filedb:add-path-stat db path parent statinfo)
  (let ((stmt (sqlite3:prepare db "INSERT INTO paths (path,parent_id,mode,uid,gid,size,mtime) VALUES (?,?,?,?,?,?,?);")))
	(sqlite3:execute stmt
			 path
			 parent
			 (vector-ref statinfo 1) ;; mode
			 (vector-ref statinfo 3) ;; uid
			 (vector-ref statinfo 4) ;; gid
			 (vector-ref statinfo 5) ;; size
			 (vector-ref statinfo 8) ;; mtime
			 )
	(sqlite3:finalize! stmt))) ;;  (filedb:get-current-time-string))))
  
(define (filedb:add-path db path parent)
  (let ((stmt (sqlite3:prepare db "INSERT INTO paths (path,parent_id) VALUES (?,?);")))
    (sqlite3:execute stmt path parent)
    (sqlite3:finalize! stmt)))

(define (filedb:register-path fdb path #!key (save-stat #f))
  (let* ((db        (filedb:fdb-get-db        fdb))
	 (pathcache (filedb:fdb-get-pathcache fdb))
	 (stat      (if save-stat (file-stat path #t)))
	 (id        (hash-table-ref/default pathcache path #f)))
    (if id id 
        (let ((plist (string-split path "/")))
          (let loop ((head (car plist))
                     (tail (cdr plist))
                     (parent 0))
            (let ((id (filedb:get-path-id db head parent))
                  (done (null? tail)))
              (if id          ;; we'll have a id if the path is already registered
                  (if done 
                      (begin
                        (hash-table-set! pathcache path id)
                        id) ;; return the last path id for a result
                      (loop (car tail)(cdr tail) id))
                  (begin      ;; add the path and then repeat the loop with the same data
		    (if save-stat
			(filedb:add-path-stat db head parent stat) 
			(filedb:add-path db head parent))
                    (loop head tail parent)))))))))

(define (filedb:update-recursively fdb path #!key (save-stat #f))
  (let ((p  (open-input-pipe (string-append "find -L " path)))) ;; (resolve-pathname path)))) ;; (string-append "find " path))))
    (print "processed 0 files...")
    (let loop ((l  (read-line p))
               (lc 0)) ;; line count
      (if (eof-object? l) 
	  (begin
	    (print "          " lc " files")
	    (close-input-port p))
          (begin
            (filedb:register-path fdb l save-stat: save-stat) ;; (get-real-path l)) ;; don't like losing the original path info
            (if (= (modulo lc 100) 0)
                (print "          " lc " files"))
            (loop (read-line p)(+ lc 1)))))))

(define (filedb:update fdb path #!key (save-stat #f))
  ;; first get the realpath and add it to the bases table
  (let ((real-path path) ;; (filedb:get-real-path path))
	(db        (filedb:fdb-get-db    fdb)))
    (filedb:add-base db real-path)
    (filedb:update-recursively fdb path save-stat: save-stat)))

;; not used and broken
;;
(define (filedb:get-real-path path)
  (let* ((p (open-input-pipe (string-append real-path " " (regexp-escape path))))
         (pth (read-line p)))
    (if (eof-object? pth) path
	(begin
	  (close-input-port p)
	  pth))))

(define (filedb:drop-base fdb path)
  (print "Sorry, I don't do anything yet"))

(define (filedb:find-all fdb pattern action)
  (let* ((db     (filedb:fdb-get-db fdb))
	 (stmt   (sqlite3:prepare db "SELECT id FROM paths WHERE path like ?;"))
	 (result '()))
    (sqlite3:for-each-row 
     (lambda (num)
       (action num)
       (set! result (cons num result))) stmt pattern)
    (sqlite3:finalize! stmt)
    result))

(define (filedb:get-path-record fdb id)
  (let* ((db        (filedb:fdb-get-db        fdb))
	 (partcache (filedb:fdb-get-partcache fdb))
	 (dat (hash-table-ref/default partcache id #f)))
    (if dat dat
	(let ((stmt (sqlite3:prepare db "SELECT path,parent_id FROM paths WHERE id=?;"))
	      (result #f))
	  (sqlite3:for-each-row 
	   (lambda (path parent_id)(set! result (list path parent_id))) stmt id)
	  (hash-table-set! partcache id result)
	  (sqlite3:finalize! stmt)
	  result))))

(define (filedb:get-children fdb parent-id)
  (let* ((db        (filedb:fdb-get-db fdb))
	 (res       '()))
    (sqlite3:for-each-row
     (lambda (id path parent-id)
       (set! res (cons (vector id path parent-id) res)))
     db "SELECT id,path,parent_id FROM paths WHERE parent_id=?;"
     parent-id)
    res))

;; retrieve all that have children and those without
;; children that match patt
(define (filedb:get-children-patt fdb parent-id search-patt)
  (let* ((db        (filedb:fdb-get-db fdb))
	 (res       '()))
    ;; first get the children that have no children
    (sqlite3:for-each-row
     (lambda (id path parent-id)
       (set! res (cons (vector id path parent-id) res)))
     db "SELECT id,path,parent_id FROM paths WHERE parent_id=? AND 
            (id IN (SELECT parent_id FROM paths) OR path LIKE ?);"
     parent-id search-patt)
    res))

(define (filedb:get-path fdb id)
  (let* ((db      (filedb:fdb-get-db      fdb))
	 (idcache (filedb:fdb-get-idcache fdb))
	 (path    (hash-table-ref/default idcache id #f)))
    (if path path
        (let loop ((curr-id id)
                   (path    ""))
          (let ((path-record (filedb:get-path-record fdb curr-id)))
            (if (not path-record) #f ;; this id has no path
                (let* ((parent-id (list-ref path-record 1))
                       (pname     (list-ref path-record 0))
                       (newpath   (string-append  "/" pname path)))
                  (if (= parent-id 0) ;; fields 0=path, 1=parent. root parent=0
                      (begin
                        (hash-table-set! idcache id newpath)
                        newpath)
                      (loop parent-id newpath)))))))))

(define (filedb:search db pattern)
  (let ((action (lambda (id)(print (filedb:get-path db id)))))
    (filedb:find-all db pattern action)))

