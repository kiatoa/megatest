;;======================================================================
;; Copyright 2006-2013, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.
;;======================================================================

;;======================================================================
;; Tests
;;======================================================================

(use sqlite3 srfi-1 posix regex regex-case srfi-69 dot-locking tcp directory-utils)
(import (prefix sqlite3 sqlite3:))

(declare (unit tests))
(declare (uses lock-queue))
(declare (uses db))
(declare (uses common))
(declare (uses items))
(declare (uses runconfig))

(include "common_records.scm")
(include "key_records.scm")
(include "db_records.scm")
(include "run_records.scm")
(include "test_records.scm")

;; Populate the links tree with index.html files
;;
;;   - start from most recent tests and work towards oldest -OR-
;;     start from deepest hierarchy and work way up
;;   - look up tests in megatest.db
;;   - cross-reference the tests to stats.db
;;   - if newer than event_time in stats.db or not registered in stats.db regenerate
;;   - run du and store in stats.db
;;   - when all tests at that level done generate next level up index.html
;; 
;;     include in rollup html index.html:
;;          sum of du
;;          counts of PASS, FAIL, RUNNING, REMOTEHOSTSTART, LAUNCHED, CHECK etc.
;;          overall status
;;
;;     include in test specific index.html:
;;          host, uname, cpu graph, disk avail graph, steps, data
;;          meta data, state, status, du
;;          