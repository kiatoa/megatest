
;; Copyright 2006-2011, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

;;  strftime('%m/%d/%Y %H:%M:%S','now','localtime')

(use sqlite3 srfi-1 posix regex regex-case srfi-69 dot-locking)
(import (prefix sqlite3 sqlite3:))

(declare (unit ezsteps))
(declare (uses db))
(declare (uses common))
(declare (uses items))
(declare (uses runconfig))

(include "common_records.scm")
; (include "key_records.scm")
; (include "db_records.scm")
; (include "run_records.scm")

