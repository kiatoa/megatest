(use posix)
(use regex)
(use directory-utils)
(use srfi-18 srfi-69 nanomsg)

(define (client-send-receive soc msg)
  (nn-send soc msg)
  (nn-recv soc))

;;do as calling user
(define (do-as-calling-user proc)
  (let ((eid (current-effective-user-id))
        (cid (current-user-id)))
    (if (not (eq? eid cid)) ;; running suid
        (set! (current-effective-user-id) cid))
    (proc)
    (if (not (eq? eid cid))
        (set! (current-effective-user-id) eid))))

;; use mutex to not open/close files at same time
;;
(define (checksum mtx file)
  (mutex-lock! mtx)
  (let-values (((inp oup pid)
                (process "shasum" (list file))))
    (mutex-unlock! mtx)
    (let ((result (read-line inp)))
      ;; now flush out remaining output
      (let loop ((inl (read-line inp)))
        (if (eof-object? inl)
            (if (string? result)
                (begin
                  (mutex-lock! mtx)
                  (close-input-port inp)
                  (close-output-port oup)
                  (mutex-unlock! mtx)
                  (car (string-split result)))
                #f)
            (loop (read-line inp)))))))

(define *max-running* 40)

(define (gather-dir-info path)
  (let ((mtx1     (make-mutex))
        (threads  (make-hash-table))
        (last-num 0)
        (req      (nn-socket 'req)))
    (print "starting client with pid " (current-process-id))
    (nn-connect req
                "tcp://localhost:5559")
    ;; "ipc:///tmp/test-ipc")
    (find-files 
     path 
     ;; test: #t
     action: (lambda (p res)
               (let ((info (cond
                            ((not (file-read-access? p)) '(cant-read))
                            ((directory? p)              '(dir))
                            ((symbolic-link? p)          (list 'symlink (read-symbolic-link p)))
                            (else                        '(data)))))
                 (if (eq? (car info) 'data)
                     (let loop ((start-time (current-seconds)))
                       (mutex-lock! mtx1)
                       (let* ((num-threads (hash-table-size threads))
                              (ok-to-run   (> *max-running* num-threads)))
                         ;; (if (> (abs (- num-threads last-num)) 2)
                         ;;     (begin
                         ;;       ;; (print "num-threads:" num-threads)
                         ;;       (set! last-num num-threads)))
                         (mutex-unlock! mtx1)
                         (if ok-to-run
                             (let ((run-time-start (current-seconds)))
                               ;; (print "num threads: " num-threads)
                               (let ((th1  (make-thread
                                            (lambda ()
                                              (let ((cksum (checksum mtx1 p))
                                                    (run-time (- (current-seconds) run-time-start)))
                                                (mutex-lock! mtx1)
                                                (client-send-receive req (conc p " " cksum))
                                                (mutex-unlock! mtx1))
                                              (let loop2 ()
                                                (mutex-lock! mtx1)
                                                (let ((registered (hash-table-exists? threads p)))
                                                  (if registered
                                                      (begin
                                                        ;; (print "deleting thread reference for " p)
                                                        (hash-table-delete! threads p))) ;; delete myself
                                                  (mutex-unlock! mtx1)
                                                  (if (not registered)
                                                      (begin
                                                        (thread-sleep! 0.5)
                                                        (loop2))))))
                                            p)))
                                 (thread-start! th1)
                                 ;; (thread-sleep! 0.05) ;; give things a little time to get going
                                 ;; (thread-join! th1) ;; 
                                 (mutex-lock! mtx1)
                                 (hash-table-set! threads p th1)
                                 (mutex-unlock! mtx1)
                                 )) ;; thread is launched
                             (let ((run-time (- (current-seconds) start-time))) ;; couldn't launch yet
                               (cond
                                ((< run-time 5)) ;; blast on through
                                ((< run-time 30)(thread-sleep! 0.1))
                                ((< run-time 60)(thread-sleep! 2))
                                ((< run-time 120)(thread-sleep! 3))
                                (else (thread-sleep! 3)))
                               (loop start-time)))))))))
    (map thread-join! (hash-table-values threads))
    (client-send-receive req "quit")
    (nn-close req)
    (exit)))

;; recieve and store the file data, note: this is effectively a *server*, not a client.
;;
(define (compare-directories path1 path2)
  (let ((p1dat       (make-hash-table))
        (p2dat       (make-hash-table))
        (numdone     0) ;; increment when recieved a quit. exit when > 2
        (rep         (nn-socket 'rep))
        (p1len       (string-length path1))
        (p2len       (string-length path2))
        (both-seen   (make-hash-table)))
    (nn-bind    rep  
                "tcp://*:5559")
    ;; "ipc:///tmp/test-ipc")
    ;; start clients
    (thread-sleep! 0.1)
    (system (conc "./remotediff-nmsg " path1 " &"))
    (system (conc "./remotediff-nmsg " path2 " &"))
    (let loop ((msg-in (nn-recv rep))
               (last-print 0))
      (if (equal? msg-in "quit")
          (set! numdone (+ numdone 1)))
      (if (and (not (equal? msg-in "quit"))
               (< numdone 2))
          (let* ((parts (string-split msg-in))
                 (filen (car parts))
                 (finfo (cadr parts))
                 (isp1  (substring-index path1 filen 0)) ;; is this a path1?
                 (isp2  (substring-index path2 filen 0)) ;; is this a path2?
                 (tpth  (substring filen (if isp1 p1len p2len) (string-length filen))))
            (hash-table-set! (if isp1 p1dat p2dat)
                             tpth
                             finfo)
            (if (and (hash-table-exists? p1dat tpth)
                     (hash-table-exists? p2dat tpth))
                (begin
                  (if (not (equal? (hash-table-ref p1dat tpth)
                                   (hash-table-ref p2dat tpth)))
                      (print "DIFF: " tpth))
                  (hash-table-set! both-seen tpth finfo)))
            (nn-send rep "done")
            (loop (nn-recv rep)
                  (if (> (- (current-seconds) last-print) 15)
                      (begin
                        (print "Processed " (hash-table-size p1dat) ", " (hash-table-size p2dat))
                        (current-seconds))
                      last-print)))))
    (print "p1: " (hash-table-size p1dat) " p2: " (hash-table-size p2dat))
    (hash-table-for-each
     p1dat
     (lambda (k v)
       (if (not (hash-table-exists? p2dat k))
           (print "REMOVED: " k))))
    (hash-table-for-each
     p2dat
     (lambda (k v)
       (if (not (hash-table-exists? p1dat k))
           (print "ADDED: " k))))
    (list p1dat p2dat)))

(if (< (length (argv)) 2)
    (begin
      (print "Usage: remotediff-nmsg file1 file2")
      (exit)))

(if (eq? (length (argv)) 2) ;; given a single path
    (gather-dir-info (cadr (argv)))
    (compare-directories (cadr (argv))(caddr (argv))))

(print "Done")
