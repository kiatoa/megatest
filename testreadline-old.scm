(use readline apropos)
(import readline)
(import apropos)
(gnu-history-install-file-manager
	     (string-append
	      (or (get-environment-variable "HOME") ".") "/.megatest_history"))
(current-input-port (make-gnu-readline-port "megatest> "))
;; (current-input-port (make-readline-port))
;; (install-history-file #f "/.csi.history")
(repl)
