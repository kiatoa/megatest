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
			 "\\[([a-z0-9]+)\\]\\s+"
			 "(.*)"
			 "\\s+\\(.*tags:\\s+" branch 
			 ;; ".*\\)"
			 )))
  (print "basecommit: " basecommit ", branch: " branch ", theregex: " theregex ", cmd: \"" cmd "\"")
  (with-input-from-pipe
   cmd
   (lambda ()
     (let loop ((inl (read-line))
		(res '()))
       (if (not (eof-object? inl))
	   (let ((have-match (string-search theregex inl)))
	     (if have-match
		 (loop (read-line)
		       (cons (conc "fossil merge --cherrypick " (cadr have-match)
				   "\nfossil commit -m \"Cherry pick from " (cadr have-match)
				   ": " (caddr have-match) "\"")
			     res))
		 (loop (read-line) res)))
	   (map print res))))))

;; (print "match: " inl "\n   $1: " (cadr have-match) " $2: " (caddr have-match))
;; (print "no match: " theregex " " inl))
;; (loop (read-line))))))))
