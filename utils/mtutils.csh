# Better to use the mt_* snippet scripts in utils 
#   To use the snippets set PREFIX then install with "make installall"

alias mt_runstep 'set argv=(\!*); \
 set stepname = $1;shift; \
 megatest -runstep $stepname -logpro ${stepname}.logpro "$*" || exit $?'

alias mt_laststep 'set argv=(\!*);set stepname = $1;shift; \
 megatest -runstep $stepname -logpro ${stepname}.logpro "$*" ; \
 set exitstatus = $? ; \
 if ( $exitstatus == 0) megatest -test-status :state COMPLETED :status PASS ; \
 if ( $exitstatus != 0) megatest -test-status :state COMPLETED :status FAIL ; \
 if ( $exitstatus != 0) exit $exitstatus'



