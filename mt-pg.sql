-- CREATE TABLE IF NOT EXISTS keys (
--        id SERIAL PRIMARY KEY,
--        fieldname TEXT,
--        fieldtype TEXT,
--        CONSTRAINT keyconstraint UNIQUE (fieldname));

DROP TABLE IF EXISTS areas;
DROP TABLE IF EXISTS ttype;
DROP TABLE IF EXISTS runs;
DROP TABLE IF EXISTS run_stats;
DROP TABLE IF EXISTS test_meta;
DROP TABLE IF EXISTS tasks_queue;
DROP TABLE IF EXISTS archive_disks;
DROP TABLE IF EXISTS archive_blocks;
DROP TABLE IF EXISTS archive_allocations;
DROP TABLE IF EXISTS extradat;
DROP TABLE IF EXISTS metadat;
DROP TABLE IF EXISTS access_log;
DROP TABLE IF EXISTS tests;
DROP TABLE IF EXISTS test_steps;
DROP TABLE IF EXISTS test_data;
DROP TABLE IF EXISTS test_rundat;
DROP TABLE IF EXISTS archives;

CREATE TABLE IF NOT EXISTS areas (
       id SERIAL PRIMARY KEY,
       area_name TEXT NOT NULL,
       area_path TEXT NOT NULL,
       last_sync INTEGER DEFAULT 0,
       CONSTRAINT areaconstraint UNIQUE (area_name));

INSERT INTO areas (id,area_name,area_path) VALUES (0,'local','.');

CREATE TABLE IF NOT EXISTS ttype (
       id SERIAL PRIMARY KEY,
       target_spec TEXT DEFAULT '');
       
CREATE TABLE IF NOT EXISTS runs (
       id SERIAL PRIMARY KEY,
       target     TEXT DEFAULT '',
       ttype_id   INTEGER DEFAULT 0,
       run_name    TEXT DEFAULT 'norun',
       state      TEXT DEFAULT '',
       status     TEXT DEFAULT '',
       owner      TEXT DEFAULT '',
       event_time INTEGER DEFAULT extract(epoch from now()),
       comment    TEXT DEFAULT '',
       fail_count INTEGER DEFAULT 0,
       pass_count INTEGER DEFAULT 0,
       last_update INTEGER DEFAULT extract(epoch from now()),
       area_id     INTEGER DEFAULT 0,
       CONSTRAINT runsconstraint UNIQUE (target,ttype_id,run_name));

CREATE TABLE IF NOT EXISTS run_stats (
       id     SERIAL PRIMARY KEY,
       run_id INTEGER,
       state  TEXT,
       status TEXT,
       count  INTEGER,
       last_update INTEGER DEFAULT extract(epoch from now()));

CREATE TABLE IF NOT EXISTS test_meta (
       id          SERIAL PRIMARY KEY,
       test_name    TEXT DEFAULT '',
       author      TEXT DEFAULT '',
       owner       TEXT DEFAULT '',
       description TEXT DEFAULT '',
       reviewed    TEXT,
       iterated    TEXT DEFAULT '',
       avg_runtime REAL,
       avg_disk    REAL,
       tags        TEXT DEFAULT '',
       jobgroup    TEXT DEFAULT 'default',
       CONSTRAINT test_meta_constraint UNIQUE (test_name));

CREATE TABLE IF NOT EXISTS tasks_queue (
       id SERIAL PRIMARY KEY,
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
       id SERIAL PRIMARY KEY,
       archive_area_name TEXT,
       disk_path TEXT,
       last_df INTEGER DEFAULT -1,
       last_df_time INTEGER DEFAULT extract(epoch from now()),
       creation_time INTEGER DEFAULT extract(epoch from now()));

CREATE TABLE IF NOT EXISTS archive_blocks (
       id SERIAL PRIMARY KEY,
       archive_disk_id INTEGER,
       disk_path TEXT,
       last_du INTEGER DEFAULT -1,
       last_du_time INTEGER DEFAULT extract(epoch from now()),
       creation_time INTEGER DEFAULT extract(epoch from now()));

CREATE TABLE IF NOT EXISTS archive_allocations (
       id SERIAL PRIMARY KEY,
       archive_block_id INTEGER,
       test_name TEXT,
       item_path TEXT,
       creation_time INTEGER DEFAULT extract(epoch from now()));

CREATE TABLE IF NOT EXISTS extradat (
       id SERIAL PRIMARY KEY,
       run_id INTEGER,
       key TEXT,
       val TEXT);

CREATE TABLE IF NOT EXISTS metadat (
       id SERIAL PRIMARY KEY,
       var TEXT,
       val TEXT);

CREATE TABLE IF NOT EXISTS access_log (
       id SERIAL PRIMARY KEY,
       "user" TEXT,
       accessed TIMESTAMP,
       args TEXT);

CREATE TABLE IF NOT EXISTS tests  (
       id SERIAL PRIMARY KEY,                                                                            
       run_id       INTEGER   DEFAULT -1,                                                                 
       test_name    TEXT      DEFAULT 'noname',                                                           
       item_path    TEXT      DEFAULT '',                                                                 
       state        TEXT      DEFAULT 'NOT_STARTED',                                                      
       status       TEXT      DEFAULT 'FAIL',                                                             
       host         TEXT      DEFAULT 'n/a',                                                              
       cpuload      REAL      DEFAULT -1,                                                                 
       diskfree     INTEGER   DEFAULT -1,                                                                 
       uname        TEXT      DEFAULT 'n/a',                                                              
       rundir       TEXT      DEFAULT '/tmp/badname',                                                     
       shortdir     TEXT      DEFAULT '/tmp/badname',                                                     
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
       CONSTRAINT testsconstraint UNIQUE (run_id, test_name, item_path));  

CREATE TABLE IF NOT EXISTS test_steps (
       id SERIAL PRIMARY KEY,
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
       id SERIAL PRIMARY KEY,
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
       id           SERIAL PRIMARY KEY,
       test_id      INTEGER,
       update_time  INTEGER,
       cpuload      INTEGER DEFAULT -1,
       diskfree     INTEGER DEFAULT -1,
       diskusage    INTEGER DEFAULT -1,
       run_duration INTEGER DEFAULT 0);

CREATE TABLE IF NOT EXISTS archives (
       id           SERIAL PRIMARY KEY,
       test_id      INTEGER,
       state        TEXT DEFAULT 'new',
       status       TEXT DEFAULT 'n/a',
       archive_type TEXT DEFAULT 'bup',
       du           INTEGER,
       archive_path TEXT);
 

-- TRUNCATE archive_blocks, archive_allocations, extradat, metadat,
-- access_log, tests, test_steps, test_data, test_rundat, archives, runs,
-- run_stats, test_meta, tasks_queue, archive_disks;
