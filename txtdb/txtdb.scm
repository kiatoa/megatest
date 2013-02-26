
;; Copyright 2006-2012, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

(use ssax)

;; Read a non-compressed gnumeric file
(define (txtdb:read-gnumeric-xml fname)
  (with-input-from-file fname
    (lambda ()
      (ssax:xml->sxml (current-input-port) '()))))

;; (serialize-sxml a output: "new.xml")
