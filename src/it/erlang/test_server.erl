#!/usr/bin/env escript
%%! -sname mule_test_server -setcookie mule_test

main(_) ->
  io:format("test_server started~n"),
  register(mule_test_server, self()),
  global:register_name(mule_test_server, self()),
  loop().

loop() ->
  receive
    {Pid, stop} -> io:format("stopping test_server~n"), global:unregister_name(mule_test_server), Pid ! stopped;
    {Pid, Msg} -> Pid ! {self(), {ack, Msg}}, loop();
    Other -> io:format("ignoring: ~p~n", [Other]), loop()
  end.

