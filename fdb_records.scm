;; Single record for managing a filedb
;; make-vector-record "Filedb record" filedb fdb db dbpath pathcache idcache partcache
;; Filedb record
(define (make-filedb:fdb)(make-vector 5))
(define-inline (filedb:fdb-get-db          vec)    (safe-vector-ref  vec 0))
(define-inline (filedb:fdb-get-dbpath      vec)    (safe-vector-ref  vec 1))
(define-inline (filedb:fdb-get-pathcache   vec)    (safe-vector-ref  vec 2))
(define-inline (filedb:fdb-get-idcache     vec)    (safe-vector-ref  vec 3))
(define-inline (filedb:fdb-get-partcache   vec)    (safe-vector-ref  vec 4))
(define-inline (filedb:fdb-set-db!         vec val)(vector-set! vec 0 val))
(define-inline (filedb:fdb-set-dbpath!     vec val)(vector-set! vec 1 val))
(define-inline (filedb:fdb-set-pathcache!  vec val)(vector-set! vec 2 val))
(define-inline (filedb:fdb-set-idcache!    vec val)(vector-set! vec 3 val))
(define-inline (filedb:fdb-set-partcache!  vec val)(vector-set! vec 4 val))

;; children records, should have use something other than "child"
(define-inline (filedb:child-get-id vec)       (safe-vector-ref vec 0))
(define-inline (filedb:child-get-path vec)     (safe-vector-ref vec 1))
(define-inline (filedb:child-get-parent_id vec)(safe-vector-ref vec 2))
