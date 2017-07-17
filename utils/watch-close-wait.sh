psline=$(ps -F -u $USER | grep "mtest" |grep " -run " | egrep " -(target|reqtarg) "| head -1)
id=$(echo $psline|awk '{print $2}')
echo "Watching process for command line: $psline"
echo "  with PID=$id"
while true;do
  echo "$(lsof -n | grep CLOSE_WAIT | grep $id |grep CLOSE_WAIT | wc -l)"
  sleep 1
done
