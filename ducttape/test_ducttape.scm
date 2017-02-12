#!/usr/bin/env csi -script
(use test)
(include "ducttape-lib.scm")
(import ducttape-lib)
(import ansi-escape-sequences)
(use trace)
(set! systype (do-or-die (if (file-exists? "/bin/uname") "/bin/uname" "/usr/bin/uname")))
;(trace skim-cmdline-opts-withargs-by-regex)
;(trace keyword-skim)
;(trace re-match?)
(define (reset-ducttape)
  (unsetenv "DUCTTAPE_DEBUG_LEVEL")
  (ducttape-debug-level #f)

  (unsetenv "DUCTTAPE_DEBUG_PATTERN")
  (ducttape-debug-regex-filter ".")

  (unsetenv "DUCTTAPE_LOG_FILE")
  (ducttape-log-file #f)

  (unsetenv "DUCTTAPE_SILENT_MODE")
  (ducttape-silent-mode #f)

  (unsetenv "DUCTTAPE_QUIET_MODE")
  (ducttape-quiet-mode #f)

  (unsetenv "DUCTTAPE_COLOR_MODE")
  (ducttape-color-mode #f)
)

(define (reset-ducttape-with-cmdline-list cmdline-list)
  (reset-ducttape)

  (command-line-arguments cmdline-list)
  (ducttape-process-command-line)
)


(define (direct-iputs-test)
  (ducttape-color-mode #f)
  (ierr "I'm an error")
  (iwarn "I'm a warning")
  (inote "I'm a note")

  (ducttape-debug-level 1)
  (idbg "I'm a debug statement")
  (ducttape-debug-level #f)
  (idbg "I'm a hidden debug statement")

  (ducttape-silent-mode #t)
  (iwarn "I shouldn't show up")
  (inote "I shouldn't show up either")
  (ierr "I should show up 1")
  (ducttape-silent-mode #f)

  (ducttape-quiet-mode #t)
  (iwarn "I should show up 2")
  (inote "I shouldn't show up though")
  (ierr "I should show up 3")
  (ducttape-quiet-mode #f)

  (ducttape-debug-level 1)
  (idbg "foo")
  (iputs "dbg" "debug message")
  (iputs "e" "error message")
  (iputs "w" "warning message")
  (iputs "n" "note message")

  (ducttape-color-mode #t)
  (ierr "I'm an error COLOR")
  (iwarn "I'm a warning COLOR")
  (inote "I'm a note COLOR")
  (idbg "I'm a debug COLOR")


  )

(define (test-argprocessor-funcs)
  
  (test-group
   "Command line processor utility functions"

   (set! testargs1 '( "-d" "-d" "-d3" "-ddd" "-foo" "fooarg" "-foo" "fooarg2" "-lastArgIsDecoy" "-foo"))
   (command-line-arguments testargs1)
   (set! expected_result '("-d" "-d" "-d3" "-ddd"))
   (set! expected_sideeffect '("-foo" "fooarg" "-foo" "fooarg2" "-lastArgIsDecoy" "-foo"))

   (test "skim-cmdline-opts-noarg-by-regex result" expected_result (skim-cmdline-opts-noarg-by-regex "-d(d+|\\d+)?"))
   (test "skim-cmdline-opts-noarg-by-regex sideeffect" expected_sideeffect (command-line-arguments))


  
   (command-line-arguments testargs1)
   (set! expected_result '("fooarg" "fooarg2" ))
   (set! expected_sideeffect '( "-d" "-d" "-d3" "-ddd" "-lastArgIsDecoy" "-foo"))
   (test
    "skim-cmdline-opts-withargs-by-regex result"
    expected_result
    (skim-cmdline-opts-withargs-by-regex "--?foo"))
   
   (test
    "skim-cmdline-opts-withargs-by-regex sideeffect"
    expected_sideeffect
    (command-line-arguments))

   ))

(define (test-misc)
  (test-group
   "misc"
   (let ((tmpfile (mktemp)))
     (test-assert "mktemp: temp file created" (file-exists? tmpfile))
     (if (file-exists? tmpfile)
         (delete-file tmpfile))

     )))



(define (test-systemstuff)
  (test-group
   "system commands"

   (let-values (((ec o e) (isys (find-exe "true"))))
     (test-assert "isys: /bin/true should have exit code 0" (equal? ec 0)))
   (let-values (((ec o e) (isys (find-exe "false"))))
     (test-assert "isys: /bin/false should have exit code 1" (equal? ec 1)))

   (let-values (((ec o e) (isys "/bin/echo" "foo" "bar" "baz")))
     (test-assert "isys: /bin/echo should have exit code 0" (equal? ec 0))
     (test-assert "isys: /bin/echo should have stdout 'foo bar baz'" (equal? o "foo bar baz")))
   
   (let-values (((ec o e) (isys "/bin/ls /zzzzz")))
     (let ((expected-code
            (if (equal? systype "Darwin") 1 2))
           (expected-err
            (if (equal? systype "Darwin")
                "ls: /zzzzz: No such file or directory"
                "/bin/ls: cannot access /zzzzz: No such file or directory"))

           )
       (test "isys: /bin/ls /zzzzz should have exit code 2" expected-code ec)
       (test "isys: /bin/ls /zzzzz should have empty stdout" "" o)
       (test
        "isys: /bin/ls /zzzzz should have stderr"
        expected-err
        e))
     )

   (let-values (((ec o e) (isys "/bin/ls /etc/passwd")))
     (test "isys: /bin/ls /etc/passwd should have exit code 0" 0 ec)
     (test "isys: /bin/ls /etc/passwd should have stdout" "/etc/passwd" o)
     (test
      "isys: /bin/ls /etc/passwd should have empty stderr"
      ""
      e))

      (let ((res (do-or-die "/bin/ls /etc/passwd")))
        (test
         "do-or-die: ls /etc/passwd should work"
         "/etc/passwd" res ))

      (let ((res (do-or-die "/bin/ls /zzzzz" nodie: #t)))
        (test
         "do-or-die: ls /zzzzz should die"
         #f res ))

      ; test reading from process stdout line at a time
      (let* (
             (lineno (counter-maker))

             ; print each line with an index
             (eachline-fn (lambda (line)
                         (print "GOTLINE " (lineno) "> " line)))

             (res
              (do-or-die "/bin/ls -l /etc | head; true"
                         foreach-stdout: eachline-fn )))
        
        (test-assert "ls -l /etc should not be empty"
                     (not (equal? res ""))))
      ;; test writing to process stdout line at a time

      (let* ((tmpfile (mktemp))
             (cmd (conc "cat > " tmpfile)))
        (let-values (((c o e)
                      (isys cmd stdin-proc:
                       (lambda (myport)
                         (write-line "hello" myport)
                         (write-line "hello2" myport)
                         (close-output-port myport)))))
          (test "isys-sp: cat should exit 0" 0 c)
          (let ((mycmd (conc "cat " tmpfile)))
            (test "isys-sp: cat output should match input" "hello\nhello2" (do-or-die mycmd)))

          (delete-file tmpfile)
        ))

      (let* ((tmpfile (mktemp))
             (cmd (conc "cat > " tmpfile)))
        (do-or-die cmd stdin-proc:
                   (lambda (myport)
                     (write-line "hello" myport)
                     (write-line "hello2" myport)
                     (close-output-port myport))
                   cmd)
        (test "dod-sp: cat output should match input" "hello\nhello2" (do-or-die (conc "cat " tmpfile)))
        (delete-file tmpfile))



      

      (let*
          ((thefile (conc "/tmp/" (get-environment-variable "USER")  "9-lines"))
           (counter (counter-maker))
           (stdin-writer
            (lambda ()
              (if (< (counter) 10)
                  (number->string (counter 0))
                  #f)))
            (cmd (conc "cat > " thefile)))
        (let-values
            (((c o e)
              (isys cmd foreach-stdin-thunk: stdin-writer)))

          (test-assert "isys-fsl: cat should return 0" (equal? c 0))

          (test-assert
           "isys-fsl: cat should have written a file"
           (file-exists? thefile))
          
          (if
           (file-exists? thefile)
           (begin
             (test "isys-fsl: cat file should have right contents" "1\n2\n3\n4\n5\n6\n7\n8\n9" (do-or-die (conc "cat " thefile)))
             (delete-file thefile)))))
      
   ) ; end test-group
  ) ; end define

   
(define (test-argprocessor )
  (test-group
   "Command line processor parameter settings"

   (reset-ducttape-with-cmdline-list '())
   (test-assert "(nil) debug mode should be off" (not (ducttape-debug-level)))
   (test-assert "(nil): debug pattern should be '.'" (equal? "." (ducttape-debug-regex-filter)))
   (test-assert "(nil): colors should be off" (not (ducttape-color-mode)))
   (test-assert "(nil): silent mode should be off" (not (ducttape-silent-mode)))
   (test-assert "(nil): quiet mode should be off" (not (ducttape-quiet-mode)))
   (test-assert "(nil): logfile should be off" (not (ducttape-log-file)))

   (reset-ducttape-with-cmdline-list '("-d"))
   (test-assert "-d: debug mode should be on at level 1" (eq? 1 (ducttape-debug-level)))

   (reset-ducttape-with-cmdline-list '("-dd"))
   (test "-dd: debug level should be 2" 2 (ducttape-debug-level))

   (reset-ducttape-with-cmdline-list '("-ddd"))
   (test "-ddd: debug level should be 3" 3 (ducttape-debug-level))

   (reset-ducttape-with-cmdline-list '("-d2"))
   (test "-d2: debug level should be 2" 2 (ducttape-debug-level))

   (reset-ducttape-with-cmdline-list '("-d3"))
   (test "-d3: debug level should be 3" 3 (ducttape-debug-level))

   (reset-ducttape-with-cmdline-list '("-dp" "foo"))
   (test "-dp foo: debug pattern should be 'foo'" "foo" (ducttape-debug-regex-filter))

   (reset-ducttape-with-cmdline-list '("--debug-pattern" "foo"))
   (test "--debug-pattern foo: debug pattern should be 'foo'" "foo" (ducttape-debug-regex-filter))

   (reset-ducttape-with-cmdline-list '("-dp" "foo" "-dp" "bar"))
   (test "-dp foo -dp bar: debug pattern should be 'foo|bar'"  "foo|bar" (ducttape-debug-regex-filter))

   (reset-ducttape-with-cmdline-list '("--quiet"))
   (test-assert "-quiet: quiet mode should be active" (ducttape-quiet-mode))

   (reset-ducttape-with-cmdline-list '("--silent"))
   (test-assert "-silent: silent mode should be active" (ducttape-silent-mode))

   (reset-ducttape-with-cmdline-list '("--color"))
   (test-assert "-color: color mode should be active" (ducttape-color-mode))

   (reset-ducttape-with-cmdline-list '("--log" "foo"))
   (test "--log foo: logfile should be 'foo'" "foo" (ducttape-log-file))

))

(define (test-wwdate)
  (test-group
   "wwdate conversion tests"
   (let ((test-table
          '(("16ww01.5" . "2016-01-01")
            ("16ww18.5" . "2016-04-29")
            ("1999ww33.5" . "1999-08-13")
            ("16ww18.4" . "2016-04-28")
            ("16ww18.3" . "2016-04-27")
            ("13ww01.0" . "2012-12-30")
            ("13ww52.6" . "2013-12-28")
            ("16ww53.3" . "2016-12-28"))))
     (for-each
      (lambda (test-pair)
        (let ((wwdate (car test-pair))
              (isodate (cdr test-pair)))
          (test
           (conc "(isodate->wwdate "isodate ") => "wwdate)
           wwdate
           (isodate->wwdate isodate))
          
          (test
           (conc "(wwdate->isodate "wwdate ")   => "isodate)
           isodate
           (wwdate->isodate wwdate))))
      test-table))))

(define (main)
  ;; (test <description; #f uses func prototype> <expected result> <thunk>)
  
;  (test-group "silly settext group"
;              (test #f "\x1b[1mfoo\x1b[0m" (set-text (list 'bold) "foo"))
;              (test "settext bold" "\x1b[1mfoo\x1b[0m" (set-text (list 'bold) "foo"))
;              )

  ; visually inspect this
  (direct-iputs-test)

  ; following use unit test test-egg
  (reset-ducttape)
  (test-argprocessor-funcs)
  (reset-ducttape)
  (test-argprocessor)
  (test-systemstuff)
  (test-misc)
  (test-wwdate)
  ) ; end main()

(main)
(sendmail "brandon.j.barclay@intel.com" "6hello subject"  "test body" )

;(let* ((image-file "/nfs/site/home/bjbarcla/megatest-logo.png")
;       (cid "mtlogo")
;       (image-alist (list (cons image-file cid)))
;       (body  (conc "Hello world<br /><img cid:"cid" alt=\"test image\"><br>bye!")))

;  (sendmail "brandon.j.barclay@intel.com" "7hello subject"  body use_html: #t images-with-content-id-alist: image-alist)
;  (print "sent image mail"))
;(sendmail "bjbarcla" "2hello subject html"  "test body<h1>hello</h1><i>italics</i>" use_html: #t)
;(sendmail "bb" "4hello attach subject html"  "<h2>hmm</h2>" use_html: #t attach-files-list: '( "/Users/bb/Downloads/wdmycloud-manual-4779-705103.pdf" ) )

;(launch-repl)
(test-exit)
