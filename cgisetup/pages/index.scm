;; (require-library chicken)
;; (import chicken)

(include "../../pgdb.scm")
(declare (uses pgdb))

;; (include "src/common_records.scm")
(include "pages/index_ctrl.scm")
(define (pages:index session db shared)
  ;; (s:log " HTTP_COOKIE=" (get-environment-variable "HTTP_COOKIE"))
  (include "pages/index_view.scm")
  ;; (s:html (s:head "head")(s:body "Got here" (current-directory)))
)

