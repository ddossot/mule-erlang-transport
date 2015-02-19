#!/bin/bash

HOSTNAME=`hostname`

function test_send() {
  erl -sname "test_server_ctl@$HOSTNAME" \
      -setcookie mule_test_cookie \
      -noshell \
      -s test_server_ctl main test_send
}

echo "Compiling the erls :)"

erlc *.erl

echo "Starting Erlang test servers on $HOSTNAME"

erl -sname "test_server_ctl@$HOSTNAME" \
    -setcookie mule_test_cookie \
    -noshell \
    -s test_server_ctl main info

erl -sname "mule_test_server_node@$HOSTNAME" \
    -setcookie mule_test_cookie \
    -s test_server run \
    -noshell \
    -detached

for (( i=1; i <= 10; i++ ))
do
 test_send_result=$( test_send )

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

