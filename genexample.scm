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


(define (genexample:mk-megatest.config path)
  (let ((keystr #f)
	(keys   #f)
	(lntree #f)
	(firstd #f))
    (debug:print 0 "Note: don't worry too much about typos in this process, you will be able to edit
the generated files before starting your first runs")

    ;; first prompt user for fields
    ;;
    (debug:print 0 "First you must specify fields or keys for your megatest area. These will 
be used to organise your runs. One field should probably be \"RELEASE\". 
Other examples of useful fields might be \"PLATFORM\", \"TARGET_OS\" or if you are in the
semiconductor business perhaps things like \"TECHNOLOGY_NODE\", \"DESIGN_KIT\" or \"METAL_STACK\".

The all caps is a convention because the variables you choose will be available to your tests
as environment variables. You can edit these values later but it is generally a good idea to
settle on them and get them right early on. Your runs will be stored in directories specified by
your keys. Example, if you have keys OSFAMILY/VARIANT/OSVER/RELEASE you may get a test \"build\"
in a directory like this: linux/ubuntu/11.04/rev_1.2/build

Please enter your keys now, separated by spaces or slashes. Only alpha-numeric characters. 
Upper case recommended.")
    (set! keystr (read-line))
    (set! keys   (apply append
			(map string-split (string-split keystr "/"))))
    
    ;; Now get the link tree location and a first disk
    (debug:print 0 "Now you need an initial place to store your runs. These are called \"disks\" and you
can add more at any time. To get going provide a writeable directory name. ")
    (set! firstd (read-line))

    (debug:print 0 "Megatest uses a tree of symlinks to provide a uniform structure for finding all the tests
you run over time. Please provide a path where we can create this link tree.")
    (set! lntree (read-line))
    
    (with-output-to-file 
	(lambda ()
	  (print "[fields]")
	  (map (lambda (k)(print k " TEXT")) keys)
	  (print "")
	  (print "[setup]")
	  (print "# Adjust max_concurrent_jobs to limit how much you load your machines")
	  (print "max_concurrent_jobs 50\n")
	  (print "# This is your link path, you can move it but it is generally better to keep it stable")
	  (print "linktree " lntree)
	  (print "")))))
