#!/bin/bash

if [ -e fullrun/db/monitor.db ];then
sqlite3 fullrun/db/monitor.db << EOF
.header on
.mode column
select * from servers order by start_time desc;
.q
EOF
fi
