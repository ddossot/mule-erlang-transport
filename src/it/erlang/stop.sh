#!/bin/bash

HOSTNAME=`hostname`

echo "Stopping Erlang test servers on $HOSTNAME"

erl -sname "test_server_ctl@$HOSTNAME" \
    -setcookie mule_test_cookie \
    -noshell \
    -s test_server_ctl main stop
