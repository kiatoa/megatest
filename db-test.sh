perl -pi -e 's/db:test-set-testname!/db:test-testname-set!/g' *.scm
perl -pi -e 's/db:test-set-status!/db:test-status-set!/g' *.scm
perl -pi -e 's/db:test-set-state!/db:test-state-set!/g' *.scm
perl -pi -e 's/db:test-set-run_duration!/db:test-run_duration-set!/g' *.scm
perl -pi -e 's/db:test-set-final_logf!/db:test-final_logf-set!/g' *.scm
perl -pi -e 's/db:test-set-diskfree!/db:test-diskfree-set!/g' *.scm
perl -pi -e 's/db:test-set-cpuload!/db:test-cpuload-set!/g' *.scm

# fix few special cases
perl -pi -e 's/db:test-get-rundir-from-test-id/dbx:test-get-rundir-from-test-id/g' *scm
perl -pi -e 's/db:test-get-is-toplevel/dbx:test-is-toplevel/g' *.scm

perl -pi -e 's/db:test-get-uname/db:test-uname/g' *.scm
perl -pi -e 's/db:test-get-testname/db:test-testname/g' *.scm
perl -pi -e 's/db:test-get-status/db:test-status/g' *.scm
perl -pi -e 's/db:test-get-state/db:test-state/g' *.scm
perl -pi -e 's/db:test-get-rundir/db:test-rundir/g' *.scm
perl -pi -e 's/db:test-get-run_id/db:test-run_id/g' *.scm
perl -pi -e 's/db:test-get-run_duration/db:test-run_duration/g' *.scm
perl -pi -e 's/db:test-get-process_id/db:test-process_id/g' *.scm
perl -pi -e 's/db:test-get-pass_count/db:test-pass_count/g' *.scm
perl -pi -e 's/db:test-get-item-path/db:test-item-path/g' *.scm
perl -pi -e 's/db:test-get-id/db:test-id/g' *.scm
perl -pi -e 's/db:test-get-host/db:test-host/g' *.scm
perl -pi -e 's/db:test-get-fullname/db:test-fullname/g' *.scm
perl -pi -e 's/db:test-get-first_warn/db:test-first_warn/g' *.scm
perl -pi -e 's/db:test-get-first_err/db:test-first_err/g' *.scm
perl -pi -e 's/db:test-get-final_logf/db:test-final_logf/g' *.scm
perl -pi -e 's/db:test-get-fail_count/db:test-fail_count/g' *.scm
perl -pi -e 's/db:test-get-event_time/db:test-event_time/g' *.scm
perl -pi -e 's/db:test-get-diskfree/db:test-diskfree/g' *.scm
perl -pi -e 's/db:test-get-cpuload/db:test-cpuload/g' *.scm
perl -pi -e 's/db:test-get-comment/db:test-comment/g' *.scm
perl -pi -e 's/db:test-get-archived/db:test-archived/g' *.scm

perl -pi -e 's/dbx:test-get-rundir-from-test-id/db:test-get-rundir-from-test-id/g' *scm
perl -pi -e 's/dbx:test-is-toplevel/db:test-is-toplevel/g' *.scm

