#!/bin/bash

sqlite3 fullrun/db/monitor.db << EOF
.header on
.mode column
select * from servers;
.q
EOF
