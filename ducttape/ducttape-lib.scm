;;; ducttape-lib.meta -*- Hen -*-

((egg "ducttape-lib.egg")
 (synopsis "Tool for standard print routines and utilities for FDK Env Team.")
 (category env)
 (author "Brandon Barclay")
 (doc-from-wiki)
 (license "GPL-2")
 ;; srfi-69, posix, srfi-18
 (depends regex)
 (test-depends test)
 ; suspicious - (files "ducttape-lib")
 )
