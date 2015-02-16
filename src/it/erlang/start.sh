#!/bin/bash

echo "Starting Erlang test servers"

./test_server.erl

for (( i=1; i <= 10; i++ ))
do
 test_send_result=`./test_server_ctl.erl test_send`
 
 if [ "$test_send_result" = "send ok" ]
 then
  echo "Erlang test servers started"
  exit 0
 fi
 
 
 echo "Pondering for 1 second"
 sleep 1
done

echo "Failed to start Erlang test servers"

exit -1

