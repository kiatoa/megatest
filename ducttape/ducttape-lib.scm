(module ducttape-lib
    (
     runs-ok
     ducttape-debug-level
     ducttape-debug-regex-filter
     ducttape-silent-mode
     ducttape-quiet-mode
     ducttape-log-file
     ducttape-color-mode
     iputs-preamble
     script-name
     idbg
     ierr
     iwarn
     inote
     iputs
     re-match?
                                        ;     launch-repl
     keyword-skim
     skim-cmdline-opts-noarg-by-regex
     skim-cmdline-opts-withargs-by-regex 
     concat-lists
     process-command-line
     ducttape-append-logfile
     ducttape-activate-logfile
     isys
     do-or-die
     counter-maker
     dir-is-writable?
     mktemp
     get-tmpdir
     sendmail
     find-exe

     zeropad
     string-leftpad
     string-rightpad
     seconds->isodate
     seconds->wwdate
     seconds->wwdate-values
     isodate->seconds
     isodate->wwdate
     wwdate->seconds
     wwdate->isodate
     current-wwdate
     current-isodate
     
     )

  (import scheme chicken extras ports data-structures )
  (use posix regex ansi-escape-sequences test srfi-1 irregex slice srfi-13 rfc3339 scsh-process directory-utils uuid-lib filepath srfi-19 ) ; linenoise
  (include "mimetypes.scm") ; provides ext->mimetype
  (include "workweekdate.scm")
  (define ducttape-lib-version 1.00)
  (define (toplevel-command sym proc) (lambda () #f))
;;;; utility procedures

  ;; begin credit: megatest's process.scm
  (define (port->list fh )
    (if (eof-object? fh) #f
        (let loop ((curr (read-line fh))
                   (result '()))
          (if (not (eof-object? curr))
              (loop (read-line fh)
                    (append result (list curr)))
              result))))

  (define (conservative-read port)
    (let loop ((res ""))
      (if (not (eof-object? (peek-char port)))
          (loop (conc res (read-char port)))
          res)))
  ;; end credit: megatest's process.scm

  (define (counter-maker)
    (let ((acc 0))
      (lambda ( #!optional (increment 1) )
        (set! acc (+ increment acc))
        acc)))

  (define (port->string port #!optional ) ; todo - add newline 
    (let ((linelist (port->list port)))
      (if linelist
          (string-join linelist "\n")
          "")))


  (define (outport->foreach outport foreach-thunk)
    (let loop ((line (foreach-thunk)))
      (if line
          (begin
            (write-line line outport)
            (loop (foreach-thunk))
            )
          (begin
            ;;http://bugs.call-cc.org/ticket/766
            ;;close-[input|output]-port implicitly calling process-wait on process pipe ports. This leads to errors like
            ;;Error: (process-wait) waiting for child process failed - No child processes: 10872
            (close-output-port outport)
            #f))))
  
  ;; weird - alist-ref arg order changes signature csc vs. csi... explitly defining.
  (define (my-alist-ref key alist)
    (let ((res (assoc key alist)))
      (if res (cdr res) #f)))

  (define (keyword-skim-alist args alist)
    (let loop ((result-alist '()) (result-args args) (rest-alist alist))
      (cond
       ((null? rest-alist) (values result-alist result-args))
       (else
        (let ((keyword (caar rest-alist))
              (defval (cdar rest-alist)))
          (let-values (((kwval result-args2)
                        (keyword-skim
                         keyword
                         defval
                         result-args)))
            (loop
             (cons (cons keyword kwval) result-alist)
             result-args2
             (cdr rest-alist))))))))
  
  (define (isys command . rest-args)
    (let-values
        (((opt-alist args)
          (keyword-skim-alist
           rest-args
           '( ( foreach-stdout-thunk: . #f )
              ( foreach-stdin-thunk: . #f )
              ( stdin-proc: . #f ) ) )))
      (let* ((foreach-stdout-thunk
              (my-alist-ref foreach-stdout-thunk: opt-alist))
             (foreach-stdin-thunk
              (my-alist-ref foreach-stdin-thunk: opt-alist))
             (stdin-proc
              (if foreach-stdin-thunk
                  (lambda (port)
                    (outport->foreach port foreach-stdin-thunk))
                  (my-alist-ref stdin-proc: opt-alist))))

        ;; TODO: support command is list.
        
        (let-values (((stdout stdin pid stderr)
                      (if (null? args)
                          (process* command)
                          (process* command args))))
          
                                        ;(if foreach-stdin-thunk
                                        ;    (set! stdin-proc
                                        ;          (lambda (port)
                                        ;            (outport->foreach port foreach-stdin-thunk))))
          
          (if stdin-proc
              (stdin-proc stdin))
          
          (let ((stdout-res 
                 (if foreach-stdout-thunk  ;; don't accumulate stdout if we have a thunk; probably doing this because stdout is BIG so lets not waste memory
                     (begin
                       (port-for-each foreach-stdout-thunk (lambda () (read-line stdout)))
                       "foreach-stdout-thunk ate stdout"
                       )
                     (if stdin-proc
                         "foreach-stdin-thunk/stdin-proc blocks stdout"
                         (port->string stdout))))
                (stderr-res
                 (if stdin-proc
                     "foreach-stdin-thunk/stdin-proc blocks stdout"
                     (port->string stderr))))

            ;; if we've used a stdin-proc, we've closed stdin port, which unfortunately causes a wait-pid internally, causing stdout and stderr ports to auto-close.  don't close them again.  (so sad - we lost stdout and stderr contents when we write to stdin)
            ;; see - http://bugs.call-cc.org/ticket/766
            (if (not stdin-proc)
                (close-input-port stdout)
                (close-input-port stderr))
            
            (let-values (((anotherpid normalexit? exitstatus)  (process-wait pid)))
              (values exitstatus stdout-res stderr-res)))))))
  
  (define (do-or-die command   #!key nodie (foreach-stdout #f) (stdin-proc #f))
    (let-values (((exit-code stdout-str stderr-str) (isys command foreach-stdout-thunk: foreach-stdout stdin-proc: stdin-proc )))
      (if (equal? 0 exit-code)
          stdout-str
          (begin
            (ierr (conc "Command  > " command " "  "< failed with " exit-code " because: \n" stderr-str) )
            (if nodie #f (exit exit-code))))))




  ;; this is broken.  one day i will fix it and thus understand run/collecting... don't use isys-broken.
  (define (isys-broken  command-list)

    (let-values ( ( (rv outport errport) (run/collecting (1 2) ("ls" "-l")  ) ) ) 
      (print "rv is " rv)
      (print "op is " outport)
      (print "ep is " errport)
      (values rv (port->string outport) (port->string errport))))



  ;; runs-ok: evaluate expression while suppressing exceptions.
                                        ;    on caught exception, returns #f
                                        ;    otherwise, returns expression value
  (define (runs-ok thunk)
    (handle-exceptions exn #f (begin (thunk) #t)))

  ;; concat-lists: result list = lista + listb
  (define (concat-lists lista listb) ;; ok, I just reimplemented append...
    (foldr cons listb lista))
  

;;; setup general_lib env var parameters

  ;; show warning/note/error/debug prefixes using ansi colors
  (define ducttape-color-mode
    (make-parameter (get-environment-variable "DUCTTAPE_COLORIZE")))

  ;; if defined, has number value.  if number value > 0, show debug messages
  ;; value should be decremented in subshells -- idea is raising debug level will show debug messages deeper and deeper in process call stack
  (define ducttape-debug-level
    (make-parameter
     (let ( (raw-debug-level (get-environment-variable "DUCTTAPE_DEBUG_LEVEL")) )
       (if raw-debug-level
           (let ((num-debug-level (runs-ok (string->number raw-debug-level))))
             (if (integer? num-debug-level)
                 (begin
                   (let ((new-num-debug-level (- num-debug-level 1)))
                     (if (> new-num-debug-level 0) ;; decrement
                         (setenv "DUCTTAPE_DEBUG_LEVEL" (number->string new-num-debug-level))
                         (unsetenv "DUCTTAPE_DEBUG_LEVEL")))
                   num-debug-level) ; it was set and > 0, mode is value
                 (begin
                   (unsetenv "DUCTTAPE_DEBUG_LEVEL") ;; value was invalid, unset it
                   #f))) ; value was invalid, mode is f
           #f)))) ; var not set, mode is f


  (define ducttape-debug-mode (if (ducttape-debug-level)  #t  #f))

  ;; ducttape-debug-regex-filter suppresses non-matching debug messages
  (define ducttape-debug-regex-filter
    (make-parameter
     (let ((raw-debug-pattern (get-environment-variable "DUCTTAPE_DEBUG_PATTERN")))
       (if raw-debug-pattern
           raw-debug-pattern
           "."))))

  ;; silent mode suppresses Note and Warning type messages
  (define ducttape-silent-mode
    (make-parameter (get-environment-variable "DUCTTAPE_SILENT_MODE")))

  ;; quiet mode suppresses Note type messages
  (define ducttape-quiet-mode
    (make-parameter (get-environment-variable "DUCTTAPE_QUIET_MODE")))

  ;; if log file is defined, warning/note/error/debug messages are appended
  ;; to named logfile.
  (define ducttape-log-file
    (make-parameter (get-environment-variable "DUCTTAPE_LOG_FILE")))




  
  
;;; standard messages printing implementation

                                        ; get the name of the current script/binary being run
  (define (script-name)
    (car (reverse (string-split (car (argv)) "/"))))

  (define (ducttape-timestamp)
    (rfc3339->string (time->rfc3339 (seconds->local-time))))


  (define (iputs-preamble msg-type #!optional (suppress-color #f))
    (let ((do-color (and
                     (not suppress-color)
                     (ducttape-color-mode)
                     (terminal-port? (current-error-port)))))
      (case msg-type
        ((note)
         (if do-color
             (set-text (list 'fg-green 'bg-black 'bold) "Note:")
             "Note:"
             ))
        ((warn)
         (if do-color
             (set-text (list 'fg-yellow 'bg-black 'bold) "Warning:")
             "Warning:"
             ))
        ((err)
         (if do-color
             (set-text (list 'fg-red 'bg-black 'bold) "Error:")
             "Error:"
             ))
        ((dbg)
         (if do-color
             (set-text (list 'fg-blue 'bg-magenta) "Debug:")
             "Debug:"
             )))))

  (define (ducttape-append-logfile msg-type message #!optional (suppress-preamble #f))
    (let
        ((txt 
          (string-join 
           (list 
            (ducttape-timestamp) 
            (script-name)
            (if suppress-preamble
                message
                (string-join  (list (iputs-preamble msg-type #t) message) " ")))
           " | ")))

      (if (ducttape-log-file)
          (runs-ok
           (call-with-output-file (ducttape-log-file)
             (lambda (output-port)
               (format output-port "~A ~%" txt)
               )
             #:append))
          #t)))

  (define (ducttape-activate-logfile #!optional (logfile #f))
    ;; from python ducttape-lib.py
                                        ; message = "START - pid=%d ppid=%d argv=(%s) pwd=%s user=%s host=%s"%(pid,ppid," ".join("'"+x+"'" for x in sys.argv),os.environ['PWD'],os.getenv('USER','nouser'),os.getenv('HOST','nohost') )
    (let ((pid (number->string (current-process-id)))
          (ppid (number->string (parent-process-id)))
          (argv 
           (string-join 
            (map 
             (lambda (x) 
               (string-join (list "\"" x "\"")  "" ))
             (argv))
            " "))
          (pwd (or (get-environment-variable "PWD") "nopwd"))
          (user (or (get-environment-variable "USER") "nouser"))
          (host (or (get-environment-variable "HOST") "nohost")))
      (if logfile
          (begin
            (ducttape-log-file logfile)
            (setenv "DUCTTAPE_LOG_FILE" (ducttape-log-file))))
      (ducttape-append-logfile 'note (format #f "START - pid=~A ppid=~A argv=(~A) pwd=~A user=~A host=~A" pid ppid argv pwd user host) #t)))         

  ;; immediately activate logfile (will be noop if logfile disabled)
  (ducttape-activate-logfile)

  ;; log exit code
  (define (set-exit-handler)
    (let ((orig-exit-handler (exit-handler)))
      (exit-handler 
       (lambda (exitcode) 
         (ducttape-append-logfile 'note (format #f "Exit ~A by sys.exit" exitcode) #t)
         (orig-exit-handler exitcode)))))
  (set-exit-handler)
  
  ;; TODO: hook exception handler so we can log exception before we sign off.

  (define (idbg first-message  . rest-args)
    (let* ((debug-level-threshold
            (if (> (length rest-args) 0) (car rest-args) 1))
           (message-list
            (if (> (length rest-args) 1)
                (cons first-message (cdr rest-args))
                (list first-message)) )
           (message (apply conc
                  (map ->string message-list))))

      (ducttape-append-logfile 'dbg message)
      (if (ducttape-debug-level)
          (if (<= debug-level-threshold (ducttape-debug-level))
              (if (string-search (ducttape-debug-regex-filter) message)
                  (begin 
                    (format (current-error-port) "~A ~A (~A)~%" (iputs-preamble 'dbg) message (script-name))))))))

  (define (ierr message-first  . message-rest)
    (let* ((message
            (apply conc
             (map ->string (cons message-first message-rest)))))
      (ducttape-append-logfile 'err message)
      (format (current-error-port) "~A ~A (~A)~%" (iputs-preamble 'err) message (script-name))))

  (define (iwarn message-first  . message-rest)
    (let* ((message
            (apply conc
             (map ->string (cons message-first message-rest)))))
      (ducttape-append-logfile 'warn message)
      (if (not (ducttape-silent-mode))
          (begin
            (format (current-error-port) "~A ~A (~A)~%" (iputs-preamble 'warn) message (script-name))))))

  (define (inote message-first  . message-rest)
    (let* ((message
            (apply conc
             (map ->string (cons message-first message-rest)))))
      (ducttape-append-logfile 'note message)
      (if (not (or (ducttape-silent-mode) (ducttape-quiet-mode)))
          (begin 
            (format (current-error-port) "~A ~A (~A)~%" (iputs-preamble 'note) message (script-name))))))

  
  (define (iputs kind message #!optional (debug-level-threshold 1))
    (cond
     ((member kind (string-split "NOTE/Note/note/n/N" "/")) (inote message))
     ((member kind (string-split "Error/ERROR/error/Err/ERR/err/E/e" "/")) (ierr message))
     ((member kind
              (string-split "Warning/WARNING/warning/Warn/WARN/warn/W/w" "/"))
      (iwarn message))
     ((member kind (string-split "Debug/DEBUG/debug/Dbg/DBG/dbg/D/d" "/"))
      (idbg message debug-level-threshold))))

  (define (mkdir-recursive path-so-far hier-list-to-create)
    (if (null? hier-list-to-create)
        path-so-far
        (let* ((next-hier-item (car hier-list-to-create))
               (rest-hier-items (cdr hier-list-to-create))
               (path-to-mkdir (string-concatenate (list path-so-far "/" next-hier-item))))
          (if (runs-ok (lambda () (create-directory path-to-mkdir)))
              (mkdir-recursive path-to-mkdir rest-hier-items)
              #f))))

                                        ; ::mkdir-if-not-exists::
                                        ; make a dir recursively if it does not 
                                        ; already exist.
                                        ; on success - returns path
                                        ; on fail - returns #f
  (define (mkdirp-if-not-exists the-dir)
    (let ( (path-list (string-split the-dir "/")))
      (mkdir-recursive "/" path-list)))

                                        ; ::mkdir-if-not-exists::
                                        ; make a dir recursively if it does not 
                                        ; already exist.
                                        ; on success - returns path
                                        ; on fail - returns #f


  (define (mkdirp-if-not-exists the-dir)
    (let ( (path-list (string-split the-dir "/")))
      (mkdir-recursive "/" path-list)))

  (define (dir-is-writable? the-dir)
    (let ((dummy-file (string-concatenate (list the-dir "/.dummyfile"))))
      (and
       (file-exists? the-dir)
       (cond 
        ((runs-ok (lambda ()(with-output-to-file dummy-file (lambda () (print "foo")))))
         (begin
           (runs-ok (lambda () (delete-file dummy-file) ))
           the-dir))
        (else #f)))))


  (define (get-tmpdir )
    (let* ((tmproot
            (dir-is-writable?
             (or 
              (get-environment-variable "TMPDIR") 
              "/tmp")))

           (user
            (or
             (get-environment-variable "USER")
             "USER_Envvar_not_set"))
           (tmppath
            (string-concatenate 
             (list tmproot "/env21-general-" user ))))

      (dir-is-writable?
       (mkdirp-if-not-exists
        tmppath))))

  (define (mktemp
           #!optional
           (prefix "general_lib_tmpfile")
           (dir #f))
    (let-values
        (((fd path) 
          (file-mkstemp 
           (conc 
            (if dir  dir  (get-tmpdir))
            "/" prefix ".XXXXXX"))))
      (close-output-port (open-output-file* fd))
      path))



  ;;http://stackoverflow.com/questions/11134857/using-sendmail-for-html-body-and-binary-attachment
  ;; write send-email using:
  ;;   - isys-foreach-stdin-line
  ;;   - formatting in http://stackoverflow.com/questions/11134857/using-sendmail-for-html-body-and-binary-attachment
  (define (sendmail to_addr subject body
                    #!key
                    (from_addr "admin")
                    cc_addr
                    bcc_addr
                    more-headers
                    use_html
                    (attach-files-list '()))

    (define (sendmail-proc sendmail-port)
      (define (wl line-str)
        (write-line line-str sendmail-port))

      (define (get-uuid)
        (string-upcase (uuid->string (uuid-generate))))

      (let ((mailpart-uuid (get-uuid))
            (mailpart-body-uuid (get-uuid)))
        
        (define (boundary)
          (wl (conc "--" mailpart-uuid)))

        (define (body-boundary)
          (wl (conc "--" mailpart-body-uuid)))


        (define (email-mime-header)
          (wl (conc "From: " from_addr))
          (wl (conc "To: " to_addr))
          (if cc_addr
              (wl (conc "Cc: " cc_addr)))
          (if bcc_addr
              (wl (conc "Bcc: " bcc_addr)))
          (if more-headers
              (wl more-headers))
          (wl (conc "Subject: " subject))
          (wl "MIME-Version: 1.0")
          (wl (conc "Content-Type: multipart/mixed; boundary=\"" mailpart-uuid "\""))
          (wl "")
          (boundary)
          (wl (conc "Content-Type: multipart/alternative; boundary=\"" mailpart-body-uuid "\""))
          (wl "")
          )
        
        (define (email-text-body)
          (body-boundary)
          (wl "Content-Type: text/plain; charset=ISO-8859-1")
          (wl "Content-Disposition: inline")
          (wl "")
          (wl body)
          (body-boundary))
        
        (define (email-html-body)
          (body-boundary)
          (wl "Content-Type: text/plain; charset=ISO-8859-1")
          (wl "")
          (wl "You need to enable HTML option for email")
          (body-boundary)
          (wl "Content-Type: text/html; charset=ISO-8859-1")
          (wl "Content-Disposition: inline")
          (wl "")
          (wl body)
          (body-boundary))
        
        (define (attach-file file)
          (let* ((filename
                  (filepath:take-file-name file))
                 (ext-with-dot
                  (filepath:take-extension file))
                 (ext (string-take-right
                       ext-with-dot
                       (- (string-length ext-with-dot) 1)))
                 (mimetype (ext->mimetype ext))
                 (uuencode-command (conc "uuencode " file " " filename)))
            (boundary)
            (wl (conc "Content-Type: " mimetype "; name=\"" filename "\""))
            (wl "Content-Transfer-Encoding: uuencode")
            (wl (conc "Content-Disposition: attachment; filename=\"" filename "\""))
            (wl "")
            (do-or-die
             uuencode-command
             foreach-stdout:
             (lambda (line)
               (wl line)))))
        
        ;; send the email
        (email-mime-header)
        (if use_html
            (email-html-body)
            (email-text-body))
        (for-each attach-file attach-files-list)
        (boundary)
        (close-output-port sendmail-port)))
    
    (do-or-die "/usr/sbin/sendmail -t"
               stdin-proc: sendmail-proc))

  ;; like shell "which" command
  (define (find-exe exe)
    (let* ((path-items
            (string-split
             (or
              (get-environment-variable "PATH") "")
             ":")))

      (let loop ((rest-path-items path-items))
        (if (null? rest-path-items)
            #f
            (let* ((this-dir (car rest-path-items))
                   (next-rest (cdr rest-path-items))
                   (candidate (conc this-dir "/" exe)))
              (if (file-execute-access? candidate)
                  candidate
                  (loop next-rest)))))))




  ;; (define (launch-repl )
  ;;   (use linenoise)
  ;;   (current-input-port (make-linenoise-port))

  ;;   (let ((histfile (conc (or (get-environment-variable "HOME") ".") "/." (script-name) "-hist")))
  
  ;;     (set-history-length! 30000)
  
  ;;     (load-history-from-file histfile)
  
  ;;     (let loop ((l (linenoise "> ")))
  ;;       (cond ((equal? l "bye")
  ;;              (save-history-to-file histfile)
  ;;              "Bye!")
  ;;             ((eof-object? l)
  ;;              (save-history-to-file histfile)
  ;;              (exit))
  ;;             (else
  ;;              (display l)
  ;;              (handle-exceptions exn
  ;;                  ;;(print-call-chain (current-error-port))
  ;;                  (let ((message ((condition-property-accessor 'exn 'message) exn)))
  ;;                    (print "exn> " message )
  ;;                    ;;(pp (condition->list exn))
  ;;                    ;;(exit)
  ;;                    ;;(display "Went wrong")
  ;;                    (newline))
  ;;                (print (eval l)))))
  ;;       (newline)
  ;;       (history-add l)
  ;;       (loop (linenoise "> ")))))
  
  ;; (define (launch-repl2 )
  ;;   (use readline)
  ;;   (use apropos)
  ;;   (use trace)
  ;;   ;(import csi)
  ;;   (current-input-port (make-readline-port (conc (script-name) "> ") "... "))
  ;;  ; (install-history-file #f (conc (or (get-environment-variable "HOME") ".") "/." (script-name) "_history"))
  ;;   (parse-and-bind "set editing-mode emacs")
  ;;   (install-history-file)
  ;;   (let loop ((foo #f))

  ;;     (let ((expr (read)))
  ;;       (cond
  ;;        ((eof-object? expr) (exit))
  ;;        (else
  ;;         (handle-exceptions exn
  ;;             ;;(print-call-chain (current-error-port))
  ;;             (let ((message ((condition-property-accessor 'exn 'message) exn)))
  ;;               (print "exn> " message )
  ;;               ;;(pp (condition->list exn))
  ;;               ;;(exit)
  ;;               ;;(display "Went wrong")
  ;;               (newline))
  ;;           (print (eval expr))))))
  ;;     (loop #f))
  ;;   )

;;;; process command line options

  ;; get command line switches (have no subsequent arg; eg. [-foo])
  ;;  assumes these are switches without arguments
  ;;  will return list of matches
  ;;  removes matches from command-line-arguments parameter
  (define (skim-cmdline-opts-noarg-by-regex switch-pattern)
    (let* (
           (irr (irregex switch-pattern))
           (matches (filter
                     (lambda (x)
                       (irregex-match irr x))
                     (command-line-arguments)))
           (non-matches (filter
                         (lambda (x)
                           (not (member x matches)))
                         (command-line-arguments))))

      (command-line-arguments non-matches)
      matches))

  (define (keyword-skim keyword default args #!optional (eqpred equal?))
    (let loop ( (kwval default) (args-remaining args) (args-to-return '()) )
      (cond 
       ((null? args-remaining)
        (values
         (if (list? kwval) (reverse kwval) kwval)
         (reverse args-to-return)))
       ((and (> (length args-remaining) 1) (eqpred keyword (car args-remaining)))
        (if (list? default)
            (if (equal? default kwval)
                (loop (list (cadr args-remaining)) (cddr args-remaining) args-to-return)
                (loop (cons (cadr args-remaining) kwval) (cddr args-remaining) args-to-return))
            (loop (cadr args-remaining) (cddr args-remaining) args-to-return)))
       (else (loop kwval (cdr args-remaining) (cons (car args-remaining) args-to-return))))))



  ;; get command line switches (have a subsequent arg; eg. [-foo bar])
  ;;  assumes these are switches without arguments
  ;;  will return list of arguments to matches
  ;;  removes matches from command-line-arguments parameter

  (define (re-match? re str)
    (irregex-match re str))

  (define (skim-cmdline-opts-withargs-by-regex switch-pattern)
    (let-values
        (((result new-cmdline-args)
          (keyword-skim switch-pattern
                        '()
                        (command-line-arguments)
                        re-match?
                        )))
      (command-line-arguments new-cmdline-args)
      result))
  
  

  ;; recognize ducttape-lib command line switches (--quiet, --silent, --color, -d.., -dp.., -logfile)
  ;;    - reset parameters; reset DUCTTAPE_* env vars to match user specified intent
  ;;    - mutate (command-line-arguments) parameter to subtract these recognized and handled switches
  ;;       * beware -- now (argv) and (command-line-arguments) are inconsistent... cannot mutate (argv) alas.  Use (command-line-arguments)
  (define (process-command-line)

    ;; --quiet
    (let ((quiet-opts (skim-cmdline-opts-noarg-by-regex "--?quiet")))
      (if (not (null? quiet-opts))
          (begin
            (setenv "DUCTTAPE_QUIET_MODE" "1")
            (ducttape-quiet-mode "1"))))

    ;; --silent
    (let ((silent-opts (skim-cmdline-opts-noarg-by-regex "--?silent")))
      (if (not (null? silent-opts))
          (begin
            (setenv "DUCTTAPE_SILENT_MODE" "1")
            (ducttape-silent-mode "1"))))

    ;; -color
    (let ((color-opts (skim-cmdline-opts-noarg-by-regex "--?colou?r(ize)?")))
      (if (not (null? color-opts))
          (begin
            (setenv "DUCTTAPE_COLORIZE" "1")
            (ducttape-color-mode "1"))))

    ;; -nocolor
    (let ((nocolor-opts (skim-cmdline-opts-noarg-by-regex "--?nocolou?r(ize)?")))
      (if (not (null? nocolor-opts))
          (begin
            (unsetenv "DUCTTAPE_COLORIZE" )
            (ducttape-color-mode #f))))

    ;; -logfile
    (let ((logfile-opts (skim-cmdline-opts-withargs-by-regex "--?log(-?file)?")))
      (if (not (null? logfile-opts))
          (begin
            (ducttape-log-file (car (reverse logfile-opts)))
            (setenv "DUCTTAPE_LOG_FILE" (ducttape-log-file)))))

    ;; -d -dd -d#
    (let ((debug-opts (skim-cmdline-opts-noarg-by-regex "-d(d*|\\d+)"))
          (initial-debuglevel (if (ducttape-debug-level) (ducttape-debug-level) 0) ))
      (if (not (null? debug-opts))
          (begin
            (ducttape-debug-level
             (let loop ((opts debug-opts) (debuglevel initial-debuglevel))
               (if (null? opts)
                   debuglevel
                   (let*
                       ( (curopt (car opts))
                         (restopts (cdr opts))
                         (ds (string-match "-(d+)" curopt))
                         (dnum (string-match "-d(\\d+)" curopt)))
                     (cond
                      (ds (loop restopts (+ debuglevel (string-length (cadr ds)))))
                      (dnum  (loop restopts (string->number (cadr dnum)))))))))
            (setenv "DUCTTAPE_DEBUG_LEVEL" (number->string (ducttape-debug-level))))))


    ;; -dp <pat> / --debug-pattern <pat>
    (let ((debugpat-opts (skim-cmdline-opts-withargs-by-regex "--?(debug-pattern|dp)")))
      (if (not (null? debugpat-opts))
          (begin
            (ducttape-debug-regex-filter (string-join debugpat-opts "|"))
            (setenv "DUCTTAPE_DEBUG_PATTERN" (ducttape-debug-regex-filter)))))) 

  ;; handle command line immediately; 
  (process-command-line)                    


  ) ; end module
