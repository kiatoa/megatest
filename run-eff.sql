.mode col
.head on
select runs.runname,num_items,printf("%.2f",wall_runtime) AS runtime,printf("%.2f",max_duration) AS duration,ratio,testname from
   (select run_id,
          count(id) AS num_items,
          (max(event_time+run_duration)-min(event_time))/3600.0 AS wall_runtime,
          max(run_duration)/3600.0 AS max_duration,
          (max(event_time+run_duration)-min(event_time))/max(run_duration) AS ratio,
          testname from tests where item_path != '' AND state != 'DELETED'
          group by run_id
          order by ratio DESC) AS dat
    join runs on dat.run_id=runs.id
WHERE ratio > 1
AND runs.state != 'deleted';
