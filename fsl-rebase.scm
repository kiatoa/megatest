;; given branch and baseline commit generate list of commands to cherry pick commits
;;
;;
;; Usage: fsl-rebase basecommit branch
;;         

(use regex posix)

(let* ((basecommit (cadr (argv)))
       (branch     (caddr (argv)))
       (cmd        (conc "fossil timeline after " basecommit " -n 1000000 -W 0"))
       (theregex   (conc ;; "^[^\\]]+"
			 ;; "\\[([\\]]+)\\]\\s+"
			 ;; "(.*)"
			 "\\s+\\(.*tags:\\s+" branch 
			 ;; ".*\\)"
			 )))
  (print "basecommit: " basecommit ", branch: " branch ", theregex: " theregex ", cmd: \"" cmd "\"")
  (with-input-from-pipe
   cmd
   (lambda ()
     (let loop ((inl (read-line)))
       (if (not (eof-object? inl))
	   (let ((have-match (string-search theregex inl)))
	     (if have-match
		 (print "match: " inl)
		 (print "no match: " theregex " " inl))
	     (loop (read-line))))))))
