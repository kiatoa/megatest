psline=$(ps -F -u $USER | grep "mtest" |grep " -run " | egrep " -(target|reqtarg) "| head -1)
id=$(echo $psline|awk '{print $2}')
echo "Watching process for command line: $psline"
echo "  with PID=$id"
while true;do
  echo "CLOSE_WAIT: $(lsof -n | grep CLOSE_WAIT | grep $id | wc -l) ALL OPEN: $(lsof -n |grep $id|wc -l) ALL CLOSE_WAIT: $(netstat -ap 2> /dev/null| grep -i close_wait| wc -l)"
  sleep 1
done
