
;; Copyright 2006-2012, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

(require-extension (srfi 18) extras tcp s11n)

(use sqlite3 srfi-1 posix regex regex-case srfi-69 hostinfo md5 message-digest)
(import (prefix sqlite3 sqlite3:))

(use spiffy uri-common intarweb http-client spiffy-request-vars)

(tcp-buffer-size 2048)

(declare (unit fs-transport))

(declare (uses common))
(declare (uses db))
(declare (uses tests))
(declare (uses tasks)) ;; tasks are where stuff is maintained about what is running.

(include "common_records.scm")
(include "db_records.scm")


;;======================================================================
;; F S   T R A N S P O R T   S E R V E R
;;======================================================================

;; There is no "server" per se but a convience routine to make it non
;; necessary to be reopening the db over and over again.
;;

(define (fs:process-queue-item packet)
  (if (not *dbstruct-db*) ;; we will require that (setup-for-run) has already been called
      (set! *dbstruct-db* (db:setup-db)))
  (debug:print-info 11 *default-log-port* "fs:process-queue-item called with packet=" packet)
  (db:process-queue-item *dbstruct-db* packet))
      
