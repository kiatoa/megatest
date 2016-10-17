;;======================================================================
;; Copyright 2006-2012, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.
;;======================================================================

(declare (unit genexample))
(use posix regex)

(define genexample:example-logpro
#<<EOF
  ;; You should have at least one expect:required. This ensures that your process ran
  (expect:required in "LogFileBody" > 0 "Put description here" #/put pattern here/)
  ;;
  ;; You may need ignores to suppress false error or warning hits from the later expects
  ;; NOTE: Order is important here!
  (expect:ignore   in "LogFileBody"  < 99 "Ignore the word error in comments" #/^\/\/.*error/)
  (expect:warning  in "LogFileBody"  = 0 "Any warning" #/warn/)
  (expect:error    in "LogFileBody"  = 0 "Any error"  (list #/ERROR/ #/error/)) ;; but disallow any other errors
EOF
)

(define genexample:example-script
#<<EOF
#!/usr/bin/env bash

# Run your step here
EOF
)

(define (genexample:mk-megatest.config)
  (let ((keystr #f)
	(keys   #f)
	(lntree #f)
	(path   #f)
	(firstd #f))
    (print "Note: don't worry too much about typos in this process, you will be able to edit
the generated files before starting your first runs")

    ;; create the needed area
    (print "==================\nWhere can I create your Megatest regresssion/continuous build area? Note, your \n"
	   "tests will not necessarily be run in this area, disk space needs are modest. Current directory is:\n\n"
	   (current-directory) "\n")
    (display "Enter your megatest directory: ")
    (set! path (read-line))

    (if (not (directory? path))
	(begin
	  (print "The path " path " does not exist or is not a directory. Attempting to create it now")
	  (create-directory path #t)))

    ;; First check that the directory is empty!
    (if (and (file-exists? path)
	     (not (null? (glob (conc path "/*")))))
	(begin
	  (print "WARNING: directory " path " is not empty, are you sure you want to continue?")
	  (display "Enter y/n: ")
	  (if (equal? "y" (read-line))
	      (print "Using directory " path " for your Megatest area.")
	      (begin
		(print "INFO: Creation of megatest files in " path " aborted")
		(exit 1)))))

    ;; first prompt user for fields
    ;;
    (print
     "==================

Next you must specify fields or keys for your megatest area. These
will be used to organise your runs. One field should probably be
\"RELEASE\".  Other examples of useful fields might be \"PLATFORM\",
\"TARGET_OS\" or if you are in the semiconductor business perhaps
things like \"TECHNOLOGY_NODE\", \"DESIGN_KIT\" or \"METAL_STACK\".

The all caps is a convention because the variables you choose will be
available to your tests as environment variables. You can edit these
values later but it is generally a good idea to settle on them
early.

Your runs will be stored in directories specified by your
keys. Example, if you have keys OSFAMILY/VARIANT/OSVER/RELEASE you may
get a test \"build\" in a directory like this:
linux/ubuntu/11.04/rev_1.2/build

Please enter your keys now, separated by spaces or slashes. Only alpha-numeric characters, 
upper case recommended. Example: COMPILER_VER/RELEASE_NAME/QUAL_LEVEL
")
    (set! keys (let loop ((keystr ""))
		 (if (equal? keystr "q")
		     (begin
		       (print "Quiting ...")
		       (exit))
		     (let ((keylst (apply append
					  (map string-split (string-split keystr "/")))))
		       (if (or (null? keylst)
			       (not (null? (filter string-null? keylst))))
			   (begin
			     (display "Enter keys separated by spaces or slashes: ")
			     (loop (read-line)))
			   keylst)))))

    (print "You have choosen " (string-intersperse keys ", ") " for your keys.")
    
    ;; Now get the link tree location and a first disk
    (print
     "==================

Now you need an initial place to store your runs. These are called \"disks\" and you
can add more at any time. To get going provide a writeable directory name. 

")
    (display "Enter your test runs directory: ")
    (set! firstd (read-line))
    (if (not (directory? firstd))
	(begin
	  (print "WARNING: you have specified a path " firstd " that does not exist. Attempting to create it...\n")
	  (create-directory firstd #t)))

    (print
     "==================

Megatest uses a tree of symlinks to provide a uniform structure for finding all the tests
you run over time. Please provide a path where we can create this link tree.

")
    (display "Enter link tree directory: ")
    (set! lntree (read-line))
    (if (not (directory? lntree))
	(begin
	  (print "WARNING: you have specified a path " lntree "that does not exist. Attempting to create it...\n")
	  (create-directory lntree #t)))
    
    (with-output-to-file (conc path "/megatest.config")
      (lambda ()
	(print "# This area uses Megatest. Learn more at http://www.kiatoa.com/fossils/megatest.")
	(print "#\n")
	(print "[fields]")
	(map (lambda (k)(print k " TEXT")) keys)
	(print "")
	(print "[setup]")
	(print "# Adjust max_concurrent_jobs to limit how much you load your machines")
	(print "max_concurrent_jobs 50\n")
	(print "# This is your link path. Avoid moving it once set.")
	(print "linktree " (common:real-path lntree))
	(print "\n# Job tools are more advanced ways to control how your jobs are launched")
	(print "[jobtools]\nuseshell yes\nlauncher nbfake\nmaxload 1.5\n")
	(print "# You can override environment variables for all your tests here")
	(print "[env-override]\nEXAMPLE_VAR example value\n")
	(print "# As you run more tests you may need to add additional disks, the names are arbitrary but must be unique")
	(print "[disks]\ndisk0 " (common:real-path firstd))))

    (print
     "==================

I'm now creating a runconfigs.config file for you with a default section.
You can use this file to set variables for your tests based on the \"target\" (the combination
of keys).

")
    (with-output-to-file (conc path "/runconfigs.config")
      (lambda ()
	(print "# The variables in the default category will be seen in all runs\n[default]\nALLTESTS see this variable\n")
	
	(print "# Your variables here are grouped by targets [" (string-intersperse keys "/") "]")
	(let ((example-target (string-intersperse (map (lambda (k)(conc k "_val")) keys) "/")))
	  (print "[" example-target "]")
	  (print "ANOTHERVAR only defined if target is " example-target))
	(print "\n# It can be handy to include a file based on the users unix username.\n"
	       "# To prevent cluttering up the top level directory we'll put this file\n# in a directory called \"configs\".")
	(print "[include #{getenv USER}.config]")
	))

    (create-directory (conc path "/configs") #t)
    (with-output-to-file (conc path "/configs/" (current-user-name) ".config")
      (lambda ()
	(print "# Override settings in ../runconfigs.config for user " (current-user-name) " here.")))
    
    ;; Now create a test and logpro file
    (print
     "==================

You now have the basic common files for your megatest setup. Next run
\"megatest -gen-test\" to create a test.

Thank you for using Megatest. 

You can edit your config files and create tests in the " path " directory

")))


;;======================================================================
;; create skeleton files for a test
;;======================================================================

(define (genexample:mk-megatest-test testname)
  ;; Gather needed data
  (let ((waiton   #f)
	(priority #f)
	(description #f)
	(steps    '())
	(scripts  '())
	(items    '())
	(rel-path #f))

    (cond
     ((file-exists? "megatest.config")         (set! rel-path "./"))
     ((file-exists? "../megatest.config")      (set! rel-path "../"))
     ((file-exists? "../../megatest.config")   (set! rel-path "../../"))
     ((file-exists? "../../../megatest.config")(set! rel-path "../../../"))) ;; good enough dang it.

    ;; Don't gather data or continue if a) megatest.config can't be found or b) testconfig already exists
    (if (not rel-path)
	(begin
	  (print "ERROR: I could not find megatest.config, please run -create-test in the top dir of your megatest area")
	  (exit 1)))

    (if (file-exists? (conc rel-path "tests/" testname "/testconfig"))
	(begin
	  (print "WARNING: You already have a testconfig in " rel-path "tests/" testname ", do you want to clobber your files?")
	  (display "Enter y/n: ")
	  (if (not (equal? "y" (read-line)))
	      (begin
		(print "INFO: user abort of creation of test " testname)
		(exit 1)))))

    (print "We are going to generate a skeleton set of files for your test " testname "\n"
	   " *** Note: do not worry too much about typos, you can edit the files created when you are done.")

    (print "\n==================\nPlease describe this test. The description will be visible in various dialogs and reports")
    (display "Enter one line description for this test: ")
    (set! description (read-line))

    (print "\n\n==================\nDoes this test, " testname ", require any other test be run prior to launch?")
    (display (conc "Enter space delimited list of tests which " testname " must wait for (default is no waiton): "))
    (set! waiton (read-line))

    (print "\n\n==================\nDo you wish to prioritize the running of this test over other tests? If so")
    (print "enter a number greater than zero here")
    (display "Enter a priority of 0 (default) or higher: ")
    (set! priority (read-line))

    ;; Get the steps
    (print "\n==================\nNow to enter the one or more steps that make up your test, note; you can add more later")
    (print "Hint; use .sh extension on the script names and we'll create a placeholder script."

    (let ((stepname   #f)
	  (scriptname #f))
      (let loop ((done #f))
	(display "Enter the name for this step (blank to stop): ")
	(set! stepname (read-line))
	(if (not (equal? stepname ""))
	    (begin
	      (display "Enter the script or progam to run: ")
	      (set! scriptname (read-line))
	      (set! steps (append steps (list (list stepname scriptname))))))
	(if (not (equal? stepname ""))
	    (begin
	      (print "Added step " stepname " to list of steps.\n")
	      (loop #f)))))

    ;; Get the items
    (print "\n\n==================\nNext we need to get the variables and values you wish to iterate this test over (blank for none)")
    (let ((varname #f)
	  (values  #f))
      (let loop ((done #f))
	(display "Enter the variable name: ")
	(set! varname (read-line))
	(if (not (equal? varname ""))
	    (begin
	      (display (conc "Enter the space separated list of values for " varname ": "))
	      (set! values (read-line))
	      (set! items (append items (list (conc varname " " values))))))
	(if (not (equal? varname ""))
	    (loop #f))))

    ;; Now create the test
    (if (not rel-path)
	(begin
	  (print "ERROR: You must run this command in a megatest area under where the megatest.config file exists.")
	  (exit 1))
	(let ((testdir (conc rel-path "tests/" testname)))
	  (create-directory testdir #t)
	  (with-output-to-file (conc testdir "/testconfig")
	    (lambda ()
	      (print "# Add additional steps here. Format is \"stepname script\"\n[ezsteps]")
	      (map (lambda (stp)(print (string-intersperse stp " "))) steps)
	      (print "")
	      (print "# Test requirements are specified here\n[requirements]")
	      (print (if (string-null? waiton) "# " "") "waiton " waiton)
	      (print "priority " (if (string-null? priority) 0 priority) "\n")
	      (print "# Iteration for your test is controlled by the items section\n[items]")
	      (map print items)
	      (print "")
	      (print "# Alternatively you could use a [itemstable] section")
	      (print "# [itemstable]")
	      (print "# ITEMVAR1  a    b    c")
	      (print "# ITEMVAR2  d    e    f")
	      (print "#\n# would result in items: a/d  b/e   c/f\n#\n")
	      (print "# Logpro rules for each step can be captured here in the testconfig")
	      (print "# note: The ;; after the stepname and the leading whitespace are required")
	      (print "#")
	      (for-each (lambda (step)
			  (let ((stepname   (car step))
				(scriptname (cadr step)))
			    (print stepname " ;; rules for checking output from running step " step " with command " scriptname)
			    (print genexample:example-logpro "\n")))
			steps)
	      (print "# test_meta is a section for storing additional data on your test\n[test_meta]")
	      (print "author " (get-environment-variable "USER"))
	      (print "owner  " (get-environment-variable "USER"))
	      (print "description " description)
	      (print "tags tagone,tagtwo")
	      (print "reviewed never")))
	  ;; Now create shell scripts (if extension is .sh) and logpro files
	  (for-each (lambda (stp)
		      (let ((stepname (car stp))
			    (script   (cadr stp)))
			(if (string-match ".*\\.sh$" script)
			    (begin
			      (with-output-to-file (conc testdir "/" script)
				(lambda ()
				  (print genexample:example-script)))
			      (system (conc "chmod ug+r,a+x " (conc testdir "/" script)))))))
		    steps))))))
