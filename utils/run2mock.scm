#!/p/foundry/env/pkgs/chicken/4.10.1_v1.63/bin/csi -s
; -*- Mode: Scheme; -*-


(use ducttape-lib)
(use posix-extras pathname-expand regex matchable)
(use ini-file)
;; plugs a hole in posix-extras in latter chicken versions
(define ##sys#expand-home-path pathname-expand)
(define (realpath x) (resolve-pathname  (pathname-expand (or x "/dev/null")) ))

;; resolve fullpath to this script
(define (get-this-script-fullpath #!key (argv (argv)))
  (let* ((this-script
          (cond
           ((and (> (length argv) 2)
                 (string-match "^(.*/csi|csi)$" (car argv))
                 (string-match "^-(s|ss|sx|script)$" (cadr argv)))
            (caddr argv))
           (else (car argv))))
         (fullpath (realpath this-script)))
    fullpath))

(define *this-script-fullpath* (get-this-script-fullpath))
(define *this-script-dir* (pathname-directory *this-script-fullpath*))
(define *this-script-name* (pathname-strip-directory *this-script-fullpath*))

(define (false-on-exception thunk)
  (handle-exceptions exn #f (thunk) ))

(define (safe-file-exists? path-string)
  (false-on-exception (lambda () (file-exists? path-string))))


(define (crude-config-transformer infile outfile keep-sections-list append-text #!key (filter-patt #f))
  (let* ((inlines (with-input-from-file infile read-lines))
         (keep-lines (let loop ((lines-left inlines) (lines-out '()) (current-section #f) (section-lines-accumulator '()))
                       (let* ((this-line (if (not (null? lines-left))
                                             (car lines-left)
                                             ""))
                              (section-match (string-match "^\\s*\\[([^\\]]+)\\].*" this-line)))
                         (cond
                          ((null? lines-left)
                           (if (member current-section keep-sections-list)
                               (append lines-out (reverse section-lines-accumulator))
                               lines-out))
                          (section-match
                           (let* ((next-lines-left      (cdr lines-left))
                                  (next-lines-out       (if (member current-section keep-sections-list)
                                                            (append lines-out (reverse section-lines-accumulator))
                                                            lines-out))
                                  (next-current-section (cadr section-match))
                                  (next-section-lines-accumulator (list this-line)))
                             (loop next-lines-left next-lines-out next-current-section next-section-lines-accumulator)))
                          (else
                           (let* ((next-lines-left       (cdr lines-left))
                                  (next-lines-out        lines-out)
                                  (next-current-section  current-section)
                                  (next-section-lines-accumulator
                                   (cond
                                    ((and filter-patt (string-match (conc "^.*"filter-patt".*$") this-line))
                                     section-lines-accumulator)
                                    (else (cons this-line section-lines-accumulator)))))
                             (loop next-lines-left next-lines-out next-current-section next-section-lines-accumulator))))))))
    (with-output-to-file outfile (lambda ()
                                   (print (string-join keep-lines "\n"))
                                   (print)
                                   (print append-text)
                                   (print)))))
                                        
                           
(define (testconfig-transformer infile outfile)
  (crude-config-transformer
   infile
   outfile
   '("meta" "items" "requirements" "test_meta")
   
   (conc "

[ezsteps]
alwayspass /bin/true

")))





(let* ((mtexe         "/p/foundry/env/pkgs/megatest/1.64/31/bin/megatest")
       (faux-mtra     "/p/fdk/gwa/bjbarcla/issues/mtdev/ch/cap/faux")
       (src-mtra      "/nfs/pdx/disks/icf_fdk_asic_gwa002/asicfdkqa/fossil/megatestqa/afdkqa")
       (target        "p1275/5/ADF_r0.7_s/9p27t_tp0")
       (run           "ww38.4")
       (src-mtdb      (conc src-mtra "/megatest.db"))
       (extra-src-testdirs   '("/p/fdk/gwa/asicfdkqa/fossil/ext/afdkqa_ext/trunk/tests"))
       (mtconf        (with-input-from-pipe (conc "cd "src-mtra" && "mtexe" -show-config -target "target) read))
       (runconf       (with-input-from-pipe (conc "cd "src-mtra" && "mtexe" -show-runconfig -dumpmode sexp -target "target) read))
       (testdir-alist (alist-ref "tests-paths" mtconf equal?))
       (testdirs      (filter safe-file-exists?
                              (append extra-src-testdirs
                                      (list (conc src-mtra "/tests"))
                                      (if (and testdir-alist (not (null? testdir-alist)))
                                          (map cadr testdir-alist)
                                          '()))))
       (tconfigfiles
        (apply append (map (lambda (src-testdir)
                             (with-input-from-pipe (conc "ls -1 "src-testdir"/*/testconfig") read-lines))
                           testdirs)))
       (tconf-alist   (filter identity
                              (map (lambda (tcfile)
                                     (let* ((m (string-match "^.*/([^/]+)/testconfig$" tcfile)))
                                       (if (not (null? m))
                                           (cons (cadr m) tcfile)
                                           #f)))
                                   tconfigfiles))))

;  (pp mtconf)
;  (pp (list 'FOO testdir-alist)) (exit 1)
  ;; make megatest area
  (when (not (file-exists? src-mtdb))
    (ierr "Source does not exist.  Aborting.  [src-mtdb]")
    (exit 1))
  
  (when (file-exists? faux-mtra)
    (system (conc "cd "faux-mtra" && rm -rf $(/p/foundry/env/bin/mttmpdir)"))
    (system (conc "rm -rf "faux-mtra)))
  
  (system (conc "mkdir -p "faux-mtra))
  (system (conc "mkdir -p "faux-mtra"/links"))
  (system (conc "mkdir -p "faux-mtra"/disk0"))

  (system (conc "cd "src-mtra" && "mtexe" -show-config -target "target" -dumpmode ini > "faux-mtra"/megatest.config.in"))
  (crude-config-transformer
   (conc faux-mtra"/megatest.config.in")
   (conc faux-mtra"/megatest.config")
   '("fields" "server" "env-override" "dashboard" "validvalues")
    (conc "[setup]
linktree "faux-mtra"/links
max_concurrent_jobs 1000
launch-delay 5
use-wal 1

" ;; emacs has trouble if a string has [ at the beginning of line, so breaking it up.
"[disks]
disk0 "faux-mtra"/disk0")
    filter-patt: "MT_LINKTREE"
    )

  
  (system (conc "cd "src-mtra" && "mtexe" -show-runconfig -target "target" -dumpmode ini > "faux-mtra"/runconfigs.config"))


  (system (conc "mkdir -p "faux-mtra"/tests"))

  (for-each (lambda (tpair)
              (pp tpair)
              (let* ((testname (car tpair))
                     (src-tconfigfile (cdr tpair))
                     (destdir (conc faux-mtra"/tests/"testname)))
                (do-or-die (conc "mkdir -p "destdir))
                (do-or-die (conc "cp "src-tconfigfile" "destdir"/testconfig.in"))
                (testconfig-transformer
                 (conc destdir"/testconfig.in")
                 (conc destdir"/testconfig"))
                (print "processed test "testname)))
            tconf-alist)
  
  
  )
