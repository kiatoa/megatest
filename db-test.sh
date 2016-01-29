# perl -pi -e 's/db:test-set-testname!/db:test-testname-set!/g' *.scm
# perl -pi -e 's/db:test-set-status!/db:test-status-set!/g' *.scm
# perl -pi -e 's/db:test-set-state!/db:test-state-set!/g' *.scm
# perl -pi -e 's/db:test-set-run_duration!/db:test-run_duration-set!/g' *.scm
# perl -pi -e 's/db:test-set-final_logf!/db:test-final_logf-set!/g' *.scm
# perl -pi -e 's/db:test-set-diskfree!/db:test-diskfree-set!/g' *.scm
# perl -pi -e 's/db:test-set-cpuload!/db:test-cpuload-set!/g' *.scm

# fix few special cases
perl -pi -e 's/db:test-get-rundir-from-test-id/dbx:test-get-rundir-from-test-id/g' *scm
perl -pi -e 's/db:test-get-is-toplevel/dbx:test-is-toplevel/g' *.scm

perl -pi -e 's/db:test-uname/dbr:test-uname/g' *.scm
perl -pi -e 's/db:test-testname/dbr:test-testname/g' *.scm
perl -pi -e 's/db:test-status/dbr:test-status/g' *.scm
perl -pi -e 's/db:test-state/dbr:test-state/g' *.scm
perl -pi -e 's/db:test-rundir/dbr:test-rundir/g' *.scm
perl -pi -e 's/db:test-run_id/dbr:test-run_id/g' *.scm
perl -pi -e 's/db:test-run_duration/dbr:test-run_duration/g' *.scm
perl -pi -e 's/db:test-process_id/dbr:test-process_id/g' *.scm
perl -pi -e 's/db:test-pass_count/dbr:test-pass_count/g' *.scm
perl -pi -e 's/db:test-item-path/dbr:test-item-path/g' *.scm
perl -pi -e 's/db:test-id/dbr:test-id/g' *.scm
perl -pi -e 's/db:test-host/dbr:test-host/g' *.scm
perl -pi -e 's/db:test-fullname/dbr:test-fullname/g' *.scm
perl -pi -e 's/db:test-first_warn/dbr:test-first_warn/g' *.scm
perl -pi -e 's/db:test-first_err/dbr:test-first_err/g' *.scm
perl -pi -e 's/db:test-final_logf/dbr:test-final_logf/g' *.scm
perl -pi -e 's/db:test-fail_count/dbr:test-fail_count/g' *.scm
perl -pi -e 's/db:test-event_time/dbr:test-event_time/g' *.scm
perl -pi -e 's/db:test-diskfree/dbr:test-diskfree/g' *.scm
perl -pi -e 's/db:test-cpuload/dbr:test-cpuload/g' *.scm
perl -pi -e 's/db:test-comment/dbr:test-comment/g' *.scm
perl -pi -e 's/db:test-archived/dbr:test-archived/g' *.scm

perl -pi -e 's/dbx:test-get-rundir-from-test-id/db:test-get-rundir-from-test-id/g' *scm
perl -pi -e 's/dbx:test-is-toplevel/db:test-is-toplevel/g' *.scm

