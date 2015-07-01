;;======================================================================
;; itemwait, itemmatch

(db:compare-itempaths ref-item-path item-path itemmap)

;; prereqs-not-met

(rmt:get-prereqs-not-met run-id waitons item-path mode: testmode itemmap: itemmap))

	 (fails           (runs:calc-fails prereqs-not-met))
	 (prereq-fails    (runs:calc-prereq-fail prereqs-not-met))
	 (non-completed   (runs:calc-not-completed prereqs-not-met))
	 (runnables       (runs:calc-runnable prereqs-not-met)))
