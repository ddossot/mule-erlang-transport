#!/usr/bin/env escript
%%! -sname mule_test_server_ctl -setcookie mule_test

main(["stop"]) -> stop();
main(_) -> usage().

usage() ->
  io:format("usage: test_server stop~n"),
  halt(1).

stop() ->
  net_adm:ping(list_to_atom("mule_test_server@" ++ net_adm:localhost())),
  get_test_server_pid() ! {self(), stop},

  receive
    stopped -> io:format("test_server stopped~n")
  after
    1000 -> halt(1)
  end.

get_test_server_pid() ->
  get_test_server_pid(global:whereis_name(mule_test_server)).
get_test_server_pid(undefined) ->
  timer:sleep(100),
  get_test_server_pid();
get_test_server_pid(Pid) when is_pid(Pid) ->
  Pid.
