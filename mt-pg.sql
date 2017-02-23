CREATE TABLE IF NOT EXISTS keys (
       id INTEGER PRIMARY KEY,
       fieldname TEXT,
       fieldtype TEXT,
       CONSTRAINT keyconstraint UNIQUE (fieldname));
		    
CREATE TABLE IF NOT EXISTS areas (
       id INTEGER PRIMARY KEY,
       areaname TEXT DEFAULT 'local',
       areapath TEXT DEFAULT '.',
       last_sync INTEGER DEFAULT 0,
       CONSTRAINT areaconstraint UNIQUE (areaname));

INSERT INTO areas (id,areaname,areapath) VALUES (0,'local','.');

CREATE TABLE IF NOT EXISTS ttype (
       id INTEGER PRIMARY KEY,
       target_spec TEXT DEFAULT '');
       
CREATE TABLE IF NOT EXISTS runs (
       id INTEGER PRIMARY KEY,
       target     TEXT DEFAULT '',
       ttype_id   INTEGER DEFAULT 0,
       runname    TEXT DEFAULT 'norun',
       state      TEXT DEFAULT '',
       status     TEXT DEFAULT '',
       owner      TEXT DEFAULT '',
       event_time INTEGER DEFAULT extract(epoch from now()),
       comment    TEXT DEFAULT '',
       fail_count INTEGER DEFAULT 0,
       pass_count INTEGER DEFAULT 0,
       last_update INTEGER DEFAULT extract(epoch from now()),
       area_id     INTEGER DEFAULT 0,
       CONSTRAINT runsconstraint UNIQUE (runname));

CREATE TABLE IF NOT EXISTS run_stats (
       id     INTEGER PRIMARY KEY,
       run_id INTEGER,
       state  TEXT,
       status TEXT,
       count  INTEGER,
       last_update INTEGER DEFAULT extract(epoch from now()));

CREATE TABLE IF NOT EXISTS test_meta (
       id          INTEGER PRIMARY KEY,
       testname    TEXT DEFAULT '',
       author      TEXT DEFAULT '',
       owner       TEXT DEFAULT '',
       description TEXT DEFAULT '',
       reviewed    TEXT,
       iterated    TEXT DEFAULT '',
       avg_runtime REAL,
       avg_disk    REAL,
       tags        TEXT DEFAULT '',
       jobgroup    TEXT DEFAULT 'default',
       CONSTRAINT test_meta_constraint UNIQUE (testname));

CREATE TABLE IF NOT EXISTS tasks_queue (
       id INTEGER PRIMARY KEY,
       action TEXT DEFAULT '',
       owner TEXT,
       state TEXT DEFAULT 'new',
       target TEXT DEFAULT '',
       name TEXT DEFAULT '',
       testpatt TEXT DEFAULT '',
       keylock TEXT,
       params TEXT,
       creation_time INTEGER DEFAULT extract(epoch from now()),
       execution_time INTEGER);

CREATE TABLE IF NOT EXISTS archive_disks (
       id INTEGER PRIMARY KEY,
       archive_area_name TEXT,
       disk_path TEXT,
       last_df INTEGER DEFAULT -1,
       last_df_time INTEGER DEFAULT extract(epoch from now()),
       creation_time INTEGER DEFAULT extract(epoch from now()));

CREATE TABLE IF NOT EXISTS archive_blocks (
       id INTEGER PRIMARY KEY,
       archive_disk_id INTEGER,
       disk_path TEXT,
       last_du INTEGER DEFAULT -1,
       last_du_time INTEGER DEFAULT extract(epoch from now()),
       creation_time INTEGER DEFAULT extract(epoch from now()));

CREATE TABLE IF NOT EXISTS archive_allocations (
       id INTEGER PRIMARY KEY,
       archive_block_id INTEGER,
       testname TEXT,
       item_path TEXT,
       creation_time INTEGER DEFAULT extract(epoch from now()));

CREATE TABLE IF NOT EXISTS extradat (
       id INTEGER PRIMARY KEY,
       run_id INTEGER,
       key TEXT,
       val TEXT);

CREATE TABLE IF NOT EXISTS metadat (
       id INTEGER PRIMARY KEY,
       var TEXT,
       val TEXT);

CREATE TABLE IF NOT EXISTS access_log (
       id INTEGER PRIMARY KEY,
       "user" TEXT,
       accessed TIMESTAMP,
       args TEXT);

CREATE TABLE tests  (
       id INTEGER PRIMARY KEY,                                                                            
       run_id       INTEGER   DEFAULT -1,                                                                 
       testname     TEXT      DEFAULT 'noname',                                                           
       host         TEXT      DEFAULT 'n/a',                                                              
       cpuload      REAL      DEFAULT -1,                                                                 
       diskfree     INTEGER   DEFAULT -1,                                                                 
       uname        TEXT      DEFAULT 'n/a',                                                              
       rundir       TEXT      DEFAULT '/tmp/badname',                                                     
       shortdir     TEXT      DEFAULT '/tmp/badname',                                                     
       item_path    TEXT      DEFAULT '',                                                                 
       state        TEXT      DEFAULT 'NOT_STARTED',                                                      
       status       TEXT      DEFAULT 'FAIL',                                                             
       attemptnum   INTEGER   DEFAULT 0,                                                                  
       final_logf   TEXT      DEFAULT 'logs/final.log',                                                   
       logdat       TEXT      DEFAULT '',                                                                 
       run_duration INTEGER   DEFAULT 0,                                                                  
       comment      TEXT      DEFAULT '',                                                                 
       event_time   INTEGER DEFAULT extract(epoch from now()),                                             
       fail_count   INTEGER   DEFAULT 0,                                                                  
       pass_count   INTEGER   DEFAULT 0,                                                                  
       archived     INTEGER   DEFAULT 0, -- 0=no, > 1=archive block id where test data can be found       
       last_update  INTEGER DEFAULT extract(epoch from now()),                                               
       CONSTRAINT testsconstraint UNIQUE (run_id, testname, item_path));  

CREATE TABLE IF NOT EXISTS test_steps (
       id INTEGER PRIMARY KEY,
       test_id INTEGER, 
       stepname TEXT, 
       state TEXT DEFAULT 'NOT_STARTED', 
       status TEXT DEFAULT 'n/a',
       event_time INTEGER DEFAULT extract(epoch from now()),
       comment TEXT DEFAULT '',
       logfile TEXT DEFAULT '',
       last_update  INTEGER DEFAULT extract(epoch from now()),
       CONSTRAINT test_steps_constraint UNIQUE (test_id,stepname,state));

CREATE TABLE IF NOT EXISTS test_data (
       id INTEGER PRIMARY KEY,
       test_id INTEGER,
       category TEXT DEFAULT '',
       variable TEXT,
       value REAL,
       expected REAL,
       tol REAL,
       units TEXT,
       comment TEXT DEFAULT '',
       status TEXT DEFAULT 'n/a',
       type TEXT DEFAULT '',
       last_update  INTEGER DEFAULT extract(epoch from now()),
       CONSTRAINT test_data_constraint UNIQUE (test_id,category,variable));

CREATE TABLE IF NOT EXISTS test_rundat (
       id           INTEGER PRIMARY KEY,
       test_id      INTEGER,
       update_time  INTEGER,
       cpuload      INTEGER DEFAULT -1,
       diskfree     INTEGER DEFAULT -1,
       diskusage    INTEGER DEFAULT -1,
       run_duration INTEGER DEFAULT 0);

CREATE TABLE IF NOT EXISTS archives (
       id           INTEGER PRIMARY KEY,
       test_id      INTEGER,
       state        TEXT DEFAULT 'new',
       status       TEXT DEFAULT 'n/a',
       archive_type TEXT DEFAULT 'bup',
       du           INTEGER,
       archive_path TEXT);
 

TRUNCATE archive_blocks, archive_allocations, extradat, metadat, access_log, tests, test_steps, test_data, test_rundat, archives, keys, runs, run_stats, test_meta, tasks_queue, archive_disks;
