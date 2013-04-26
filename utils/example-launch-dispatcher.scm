
(let ((target (assoc 
	       ;; Put the variable name here, note: only *one* '
	       ;; 'TARGET_OS
	       'MANYITEMS
	       (read (open-input-string (get-environment-variable "MT_ITEM_INFO"))))))
  (case (if target target 'var-undef)
    ((suse)      (system "echo suse-launcher.pl"))
    ((redhat)    (system "echo red-hat-launcher.pl"))
    ((af)        (system "echo Got af"))
    ((var-undef) (system "echo Variable not in MT_ITEM_INFO list"))
    (else        (system "echo normal-launcher.pl"))))
