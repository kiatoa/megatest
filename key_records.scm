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

(define-inline (keys->valslots keys) ;; => ?,?,? ....
  (string-intersperse (map (lambda (x) "?") keys) ","))

(define-inline (keys->key/field keys . additional)
  (string-join (map (lambda (k)(conc k " TEXT"))
		    (append keys additional)) ","))

(define-inline (item-list->path itemdat)
  (if (list? itemdat)
      (string-intersperse  (map cadr itemdat) "/")
      ""))

