#!/bin/sh

sqlite3 megatest.db <<EOF
create view all_tests as select 
    runname,RELEASE,DOTPROC,TECH,t.testname,description,
    item_path,t.state,t.status,
    attemptnum,final_logf,logdat,run_duration,r.comment,
    t.event_time,expected_value,value,tol,tol_perc,
    first_err,first_warn,tm.tags,
    r.owner,t.comment,
    author,tm.owner,reviewed,iterated,avg_runtime,
    diskfree,uname,rundir,avg_disk,t.tags,run_id,
    host,cpuload
 from tests as t inner join runs as r on t.run_id=r.id inner join test_meta as tm on tm.testname=t.testname;
.head on
.mode csv
.output "all-data.csv"
select * from all_tests;
drop view all_tests;
.q
EOF